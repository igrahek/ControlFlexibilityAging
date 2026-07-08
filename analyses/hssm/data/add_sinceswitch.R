# Attach the interval-level "intervals since switch" predictor (SinceSwitch) to the EXISTING
# canonical varying-blocks DDM data, in place (adds one column; all other values byte-preserved,
# so Model3 is unaffected and Model4 becomes a strict generalization of it).
#
# Why this exists instead of just re-running Preprocessing.R: the preprocessing relies on a now
# deprecated dplyr behaviour (a length-n expression inside summarise(), the `Interval_devAccRT`
# line) that, under modern tidyverse, changes which intervals pass the outlier filter and thereby
# shifts ~6.3% of the interval-level Switch labels. So a full modern re-run does NOT byte-reproduce
# the published Switch column. To keep Model4 consistent with the published Model3, we compute
# SinceSwitch here from the *authoritative* Switch labels already in the canonical clean data
# (which retains intervalNum), then merge it onto the canonical DDM trials on same-run keys.
# The identical logic now also lives in Preprocessing.R (next to Switch) for future clean runs.
#
# Run from analyses/hssm/data/ :  Rscript add_sinceswitch.R
suppressMessages(library(dplyr))
CAP <- 4L  # cap at 4 ("4+"); long same-goal runs are rare and would have high leverage

clean <- read.csv("../../../data/ProcessedData/CleanDataSpeedAccuracy_nSubs_245.csv")
ddm   <- read.csv("DDM_Mixed_VaryingBlocks_AllSubs.csv")

# Sequential interval index within subject (clean data is in chronological row order;
# a new interval begins whenever intervalNum changes from the previous row).
clean <- clean %>%
  group_by(SubID) %>%
  mutate(interval_seq = cumsum(intervalNum != lag(intervalNum, default = -1L))) %>%
  ungroup()

# Interval-level SinceSwitch from the authoritative Switch labels:
# reset to 0 on Switch intervals (and block-first NA intervals), +1 per successive Repeat.
iv <- clean %>%
  group_by(SubID, interval_seq) %>%
  summarise(Switch = first(Switch), .groups = "drop") %>%
  arrange(SubID, interval_seq) %>%
  group_by(SubID) %>%
  mutate(switchflag = is.na(Switch) | Switch == "Switch",
         rungroup   = cumsum(switchflag)) %>%
  group_by(SubID, rungroup) %>%
  mutate(SinceSwitch = row_number() - 1L) %>%
  ungroup() %>%
  select(SubID, interval_seq, SinceSwitch)

clean <- clean %>% left_join(iv, by = c("SubID", "interval_seq"))

# Merge onto the DDM trials. Clean and DDM come from the same original run, so the shared trial
# columns are byte-identical; use a rich composite key. A small number of trials are genuinely
# indistinguishable on the DDM file's columns (same subject / within-interval position / RT /
# running time in different intervals), so their graded SinceSwitch is ambiguous; we take one value
# (distinct, first) and report how many. The binary switch/repeat boundary is then pinned exactly
# to the authoritative Switch column below, so those trials can only be off by the graded amount.
mk <- c("subj_idx","trialNum","intervalType","cong","rt_ms","srt")
key <- clean %>%
  transmute(subj_idx = SubID, trialNum, intervalType,
            cong = type, rt_ms = round(rt), srt = round(scaledRunningTime, 6), SinceSwitch)
collide <- key %>% group_by(across(all_of(mk))) %>%
  summarise(nss = n_distinct(SinceSwitch), .groups = "drop") %>% filter(nss > 1)
ambiguous_keys <- nrow(collide)
key <- key %>% distinct(across(all_of(mk)), .keep_all = TRUE)

ddm2 <- ddm %>%
  mutate(cong = congruency, rt_ms = round(rt * 1000), srt = round(scaledRunningTime, 6)) %>%
  left_join(key, by = mk) %>%
  select(-cong, -rt_ms, -srt)

# Pin the binary boundary EXACTLY to the authoritative Switch column, then cap.
ddm2$SinceSwitch[is.na(ddm2$SinceSwitch)] <- 0L
ddm2$SinceSwitch[ddm2$Switch == "Switch"] <- 0L
ddm2$SinceSwitch[ddm2$Switch == "Repeat" & ddm2$SinceSwitch == 0] <- 1L
ddm2$SinceSwitch <- pmin(ddm2$SinceSwitch, CAP)

# Verify: no NAs; SinceSwitch==0 EXACTLY matches Switch=="Switch"; row count preserved.
stopifnot(!any(is.na(ddm2$SinceSwitch)))
stopifnot(all((ddm2$SinceSwitch == 0) == (ddm2$Switch == "Switch")))
stopifnot(nrow(ddm2) == nrow(ddm))

write.csv(ddm2, "DDM_Mixed_VaryingBlocks_AllSubs.csv", row.names = FALSE)
cat("rows:", nrow(ddm2), "\nSinceSwitch distribution:\n"); print(table(ddm2$SinceSwitch))
cat("SinceSwitch==0 vs Switch=='Switch':", sum(ddm2$SinceSwitch==0), "/", sum(ddm2$Switch=="Switch"), "\n")
cat("graded-ambiguous composite keys (bounded residual):", ambiguous_keys, "\n")
