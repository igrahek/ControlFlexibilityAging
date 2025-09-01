#### About the code ####

# Pre-processing script for the aging CAC project
# Code written by: Ivan Grahek (2023-2025)

#### Clear environment, the paths, and criteria ####

# clear the environment
rm(list=ls()) 
#load packages and install them if they're not installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,psych)

# set seed
set.seed(42) 

# Critera for Exclusion 
thresh_minIntervals_performed = 64  # Total intervals = 80, threshold is 80 % 
thresh_minBlocks_performed = 13      # Total blocks per condition = 10
thresh_maxRT_performed = 3000       # Max RT (ms)
thresh_minRT_performed = 250        # Min RT (ms)
thresh_meanAcc_subj = .6
thresh_quiz_mistakes = 5           # Exclude if there are more than 5

# Data scrubbing variables
task_ISI = 250                      # constant variable set in task

#### Import the data ####

df.trial.raw = read.csv('../data/RawData/BehavioralData/2024-02-29_3.0/trialdata_reconfig_2024-02-29.csv') 
length(unique(df.trial.raw$uniqueid))

df.trial = df.trial.raw %>%
  separate(col = uniqueid, sep = ":", c("SubID","Version"), remove = FALSE) 

# # Load in the demographics data

#Define the path to the folder containing the CSV files
path = "../data/RawData/ProlificData/"

# Define the list of CSV file names (without .csv extension)
file_names = c("prolific_export_6532ce7066b8b233f811e422",
                "prolific_export_65d0e9d9f1264ee93bd93b9e",
                "prolific_export_65d0e9885cedc698fc6c79e6",
                "prolific_export_65d0e921ab9f1aa3eae5d128",
                "prolific_export_65d0e8b50760c74d931bc1bd",
                "prolific_export_65cfab8c44a35369d14ceecf",
                "prolific_export_65cf9a0bfd27b8ae6ca7fcfe",
                "prolific_export_65cf89d2b07e96b74b6e6ce0",
                "prolific_export_65ce69fe0a4a4a722129db1f",
                "prolific_export_657c6b8f4039a581200363c7",
                "prolific_export_657c6b54de2a8c143fcd4ac7",
                "prolific_export_657c6b101be01ef68febadb7",
                "prolific_export_657c6ac15c6ff57c5c3e2209",
                "prolific_export_657c6a599f9d7df24ea5ef08",
                "prolific_export_657c69cc4e2d997ec8a5eab5",
                "prolific_export_657b292a9dd29ccddfdccb24",
                "prolific_export_656e0c3d33f4be44d1f5a421",
                "prolific_export_65550017daa0e8cb609a2f31",
                "prolific_export_6556562ad6ea503cf8c1bcae")

# Import each CSV file as a separate data frame
for (i in 1:length(file_names)) {
  assign(paste0("data.prolific", i), read.csv(paste0(path, file_names[i], ".csv")))
}

data.prolific = rbind(data.prolific1, data.prolific2, data.prolific3,
                             data.prolific4, data.prolific5, data.prolific6,
                             data.prolific7, data.prolific8, data.prolific9,
                             data.prolific10, data.prolific11, data.prolific12,
                             data.prolific13, data.prolific14, data.prolific15,
                             data.prolific16, data.prolific17, data.prolific18, data.prolific19)


# count the number of participants with Status equal to "RETURNED" or "TIMED-OUT"
table(data.prolific$Status)

data.prolific = subset(data.prolific,data.prolific$Status=="APPROVED")
length(unique(data.prolific$Participant.id))

# Remove people who have not been approved on Prolific from the task data
df.trial <- df.trial %>%
  filter(SubID %in% data.prolific$Participant.id) 

# Load in the attention quiz data
df.quiz = read.csv('../data/RawData/BehavioralData/2024-02-29_3.0/questiondata_reconfig_2024-02-29.csv')   # ONly import once because it has everything (all data)

df.quiz = df.quiz %>% 
  separate(col = uniqueid, sep = ":", c("SubID","Version"), remove = FALSE) %>%
  filter(uniqueid %in% df.trial$uniqueid)

# Filter by incorrect
df.quizPassed = df.quiz %>% 
  dplyr::select(uniqueid,incorrectC1,incorrectC2) %>%
  mutate_at(
    vars(incorrectC1:incorrectC2),as.numeric) %>%
  filter(incorrectC1 < thresh_quiz_mistakes, incorrectC2 < thresh_quiz_mistakes)

#### Exclude subjects based on the attention checks and the order of the tasks in the battery ####

StartNParticipants = length(unique(df.trial$SubID)) 

# Subjects to exclude, automatically excludes if more than 2000 trials per subject. 
sub.trials<-data.frame(table(df.trial.raw$uniqueid))
sub.exclude<-sub.trials %>%
  filter(Freq>2000)

colnames(sub.exclude)[1] = "uniqueid"

ExcludeTooManyResponses = StartNParticipants-length(unique(df.trial$uniqueid))

# Remove the people who haven't passed the quizzes and remove the rows with NAs in trial numbers
df.trial <- df.trial %>%
  filter(uniqueid %in% df.quizPassed$uniqueid) %>% 
  filter(!is.na(trialNum))

length(unique(df.trial$uniqueid)) # 241 here

# Create Running Time Variable 
df.trial <- df.trial %>% 
  group_by(SubID,blockNum,intervalNum)%>% 
  mutate(ISI=ifelse(trialNum==1,0,250),
         trialPlusISI= rt+ISI,
         runningTime=cumsum(ifelse(trialNum==1,0,lag(trialPlusISI))),
         runningTimeLeft= intervalLength-runningTime)%>% 
  ungroup(SubID,blockNum,intervalNum)%>% 
  mutate(scaledRunningTime=scale(runningTime, scale = TRUE, center = TRUE))

# Add the previous trial type variable
df.trial = df.trial %>%
  group_by(SubID,Version,blockNum) %>%
  dplyr::mutate(prevtrialType = dplyr::lag(intervalType))  %>%
  ungroup(SubID,Version,blockNum)

# Previous trial type should be the same as the current one on the first trial of the first interval of a block (the first trial in a block is never a switch)
df.trial$prevtrialType[df.trial$intervalNum==1 & df.trial$trialNum==1] = df.trial$intervalType[df.trial$intervalNum==1 & df.trial$trialNum==1]

# Add the variable which groups all the same trial types since the last switch
df.trial = df.trial %>%
  group_by(SubID,Version,blockNum) %>%
  dplyr::mutate(TrialGroup = cumsum(prevtrialType!=intervalType))  %>%
  ungroup(SubID,Version,blockNum)

# Add switch repeat variable (0 is switch; 1 is repeat)
df.trial$SwitchTrial = ifelse(df.trial$intervalType==df.trial$prevtrialType,1,0)

# Add the since switch variable
df.trial = df.trial %>%
  group_by(SubID,Version,blockNum,TrialGroup) %>%
  dplyr::mutate(SinceSwitch = cumsum(SwitchTrial) * SwitchTrial)  %>%
  ungroup(SubID,Version,blockNum)

# If this is the first interval in the block, the Since Switch value should be 0 in the whole interval
df.trial$SinceSwitch[df.trial$intervalNum==1 & df.trial$trialNum==1] = 0


# Remove the fast/slow RTs and remove the subjects with too many responses
df.trial <- df.trial %>%
  filter(rt>thresh_minRT_performed)%>%
  filter(rt<thresh_maxRT_performed)%>%
  dplyr::select(-c('initrt','phase','moneyEarned')) %>% 
  filter(!uniqueid %in% sub.exclude) 

ExcludeQuiz = StartNParticipants - length(unique(df.trial$uniqueid))

#### Re-arrange the data ####

# Fix the blockType naming
df.trial$blockType = ifelse(df.trial$blockType=="Mixed","Mixed","Fixed")

# Create vector of trial number for each subject's session
tmp.trialdata<-df.trial %>% group_by(SubID) %>% 
  dplyr::summarise(numtrials=n(), .groups="keep")

df.trialNew<-data.frame()
for (subject in 1:length(unique(tmp.trialdata$SubID))) {
  
  #Plotting
  plot_data = subset(df.trial,df.trial$SubID == unique(tmp.trialdata$SubID)[subject])
  
  plot_data<-plot_data  %>% 
    dplyr::mutate(wholesessionTrialNum = row_number())
  df.trialNew<-rbind(df.trialNew,plot_data)
  
}

df.trial = df.trialNew

# Create additional variables and reorganize experimental factors
df.trial = df.trial %>% 
  mutate(AccRT=ifelse(hit==1,rt,NaN),
         ErrRT=ifelse(hit==0,rt,NaN),
         type=droplevels(factor(type)),
         congruence=as.numeric(type=='congruent'), # 1=congruent, 0=incongruent
         prevCongruence=head(c(c(NA),congruence),-1),
         prevHit=head(c(c(NA),hit),-1),
         IntervalsPerBlock = max(unique(intervalNum)),
         IntervalSessionNum = IntervalsPerBlock*(blockNum-1) + intervalNum,
         TrialSessionNum = wholesessionTrialNum,
         scaledIntervalSessionNum = scale(IntervalSessionNum, center = TRUE,scale = TRUE),
         scaledTrialSessionNum = scale(TrialSessionNum, center = TRUE, scale = TRUE),
         scaledIntervalBlockNum = scale(intervalNum, center = TRUE,scale = TRUE),
         scaledTrialIntervalNum = scale(trialNum, center = TRUE, scale = TRUE),
         scaledIntervalLength = scale(intervalLength, center = TRUE, scale = TRUE),
         log10_AccRT=log10(AccRT),
         log10_ErrRT=log10(ErrRT),
         scaled_log10_AccRT=scale(log10_AccRT, center = TRUE, scale = TRUE),
         lowerWord=tolower(distractor),
         ErrorType=case_when(
           hit==0 & response!=lowerWord~ "random",
           hit==0 & response==lowerWord ~ "automatic",
           hit==1~ "NoError"),
         AutomaticError=ifelse(ErrorType=="automatic",1,0),
         RandomError=ifelse(ErrorType=="random",1,0),
         AnyError=ifelse(ErrorType=="random",1,ifelse(ErrorType=="automatic",1,0))
  )


df.trial <- df.trial %>%
  group_by(SubID,blockNum,intervalType) %>%
  mutate("cm_AutomaticError" = cumsum(AutomaticError),
         "cm_RandomError" = cumsum(RandomError),
         "cm_AnyError" = cumsum(AnyError))%>%
  ungroup(SubID,blockNum,intervalType)

df.trial <- df.trial %>%
  group_by(SubID) %>%
  mutate("scaledcm_AutomaticError" = scale(cm_AutomaticError, center = TRUE, scale = TRUE),
         "scaledcm_RandomError" = scale(cm_RandomError , center = TRUE, scale = TRUE),
         "scaledcm_AnyError" = scale(cm_AnyError, center = TRUE, scale = TRUE))%>%
  ungroup(SubID)


#### Exclude subjects based on interval-level data ####

# Format data frame for interval-level data to do exclusion at the interval level
df.interval <- df.trial %>% 
  group_by(uniqueid,SubID,Version,blockNum,intervalNum,intervalLength,intervalType) %>% 
  dplyr::summarise(trialsPerInterval = n(),
                   IntervalISI = (trialsPerInterval-1)*task_ISI, # Calculate Total ISI per interval 
                   Interval_Acc=mean(hit,na.rm=T),
                   Interval_sum_Acc=sum(hit,na.rm=T), 
                   Interval_AccRT=mean(AccRT,na.rm=T),
                   Interval_devAccRT=((AccRT-Interval_AccRT)/Interval_AccRT),
                   Interval_log10_AccRT=mean(log10_AccRT,na.rm=T),
                   Interval_RT=mean(rt,na.rm=T),
                   Interval_Congruence=mean(congruence,na.rm=T), 
                   scaledIntervalSessionNum = mean(scaledIntervalSessionNum),
                   scaledIntervalBlockNum = mean(scaledIntervalBlockNum),
                   scaledIntervalLength = mean(scaledIntervalLength), 
                   .groups="keep") %>% ungroup() %>%
  mutate(Interval_norm_sum_Acc=((Interval_sum_Acc)/(intervalLength-IntervalISI))*1000, 
         scaledIntervalCong = scale(Interval_Congruence, center = TRUE, scale = TRUE), 
         scale_Interval_log10_AccRT=scale(Interval_log10_AccRT, center = TRUE, scale = TRUE)) %>%
  filter(abs(scale_Interval_log10_AccRT) < 3,Interval_Congruence<=0.6)

# Add the previous interval type variable
df.interval.previous = df.interval %>%
  group_by(uniqueid,SubID,Version,blockNum,intervalNum) %>%
  dplyr::summarise(intervalType = first(intervalType), n = n())%>%
  dplyr::arrange(intervalNum, .by_group = TRUE) %>%
  dplyr::mutate(previntervalType = dplyr::lag(intervalType)) 

# Add switch repeat variable
df.interval.previous$Switch = ifelse(df.interval.previous$intervalType==df.interval.previous$previntervalType,"Repeat","Switch")

# Merge with trial data
df.trial = plyr::join(df.trial,df.interval.previous, by = c("SubID", "blockNum", "intervalNum"),type="full")

# Summarize data by subject and check who we should go back to exclude at the subject level 
df.summary <- df.interval %>% 
  group_by(uniqueid) %>% 
  dplyr::summarise(Interval_Acc_subj=mean(Interval_Acc,na.rm=T),
                   Interval_RT_subj=mean(Interval_AccRT,na.rm=T),
                   nInterval=length(Interval_Acc), .groups="keep") %>%
  dplyr::filter(Interval_Acc_subj<thresh_meanAcc_subj,
                Interval_RT_subj>thresh_maxRT_performed,
                nInterval<thresh_minIntervals_performed) %>% ungroup()

df.summary.block <-  df.interval %>% 
  group_by(uniqueid,intervalType) %>% 
  dplyr::summarise(Interval_Acc_subj=mean(Interval_Acc,na.rm=T),
                   Interval_RT_subj=mean(Interval_AccRT,na.rm=T),
                   nInterval=length(Interval_Acc),
                   Interval_norm_sum_Acc = mean(Interval_norm_sum_Acc), 
                   .groups="keep") %>%
  dplyr::filter(Interval_Acc_subj<thresh_meanAcc_subj,
                Interval_RT_subj>thresh_maxRT_performed,
                nInterval<thresh_minBlocks_performed) %>% ungroup()

PreIntervalExclusionN = length(unique(df.trial$uniqueid))

# Exclude the subjects with low accuracy, low number of blocks performed, or very long RTs
df.trial = df.trial %>%
  filter(!uniqueid %in% df.summary$uniqueid)

ExcludeIntervalLevel = PreIntervalExclusionN - length(unique(df.trial$uniqueid))

#### Exclude subjects based on accuracy  ####

summary_table =  df.trial %>% 
  group_by(SubID,intervalType) %>% 
  dplyr::summarise(MeanAcc=mean(hit,na.rm=TRUE))

summary_table = subset(summary_table,intervalType=="Accuracy")

sub.exclude.accuracy = summary_table$SubID[which(summary_table$MeanAcc<0.0)]

PreAccuracyExclusionN = length(unique(df.trial$SubID))

# Exclude the subjects with low accuracy, low number of blocks performed, or very long RTs
df.trial = df.trial %>%
  filter(!SubID %in% sub.exclude.accuracy)

ExcludeAccuracy = PreAccuracyExclusionN - length(unique(df.trial$SubID))


# Exclude the subjects with no random errors in the accuracy condition
# df.trial = df.trial %>%
#   filter(!SubID %in% c("616766efa6666ba724ef75ca","59ff81fd5d06850001afeeab"))

#### Add age
StartN_PreAgeAddition = length(unique(df.trial$SubID))
df.age = data.prolific

df.age = subset(df.age,df.age$Status=="APPROVED")
df.age = subset(df.age,df.age$Age!="DATA_EXPIRED")
# df.age$Age = as.numeric(ifelse(df.age$Age=="DATA_EXPIRED",NaN,df.age$Age))
df.age<-df.age%>%
  dplyr::select(Participant.id,Age,Sex)

# Repeat the age for each trial
df.trial$Age = df.age$Age[match(df.trial$SubID,df.age$Participant.id)]
df.trial$Age = as.numeric(df.trial$Age)

df.trial$Sex = df.age$Sex[match(df.trial$SubID,df.age$Participant.id)]
# df.trial$Sex = as.numeric(df.trial$Sex)

# Add age group
# Create the age group variable
df.trial <- df.trial %>%
  mutate(AgeGroup = case_when(
    Age < 35 ~ "Young",
    Age >= 35 & Age <= 65 ~ "Middle",
    Age > 65 ~ "Old",
    TRUE ~ NA_character_
  ))

#### Add the quadratic age ####
# Take out the participants with no age
df.trial = df.trial[!is.na(df.trial$Age), ]
df.trial$Age_Years = df.trial$Age
df.trial$Age = scale(df.trial$Age,center = T,scale = T)
PolyAge = poly(df.trial$Age,2)
df.trial$Age = PolyAge[,1]
df.trial$Age_2 = PolyAge[,2]

# N removed due to expired age
NExpiredAge = StartN_PreAgeAddition - length(unique(df.trial$SubID))


#### Save out the data for the behavioral analyses ####
df.behavioral = df.trial

df.behavioral = df.trial %>%
  dplyr::select(SubID,trialNum,hit,rt,intervalType,blockType,type,Switch,previntervalType,intervalNum,scaledRunningTime,scaledIntervalLength,scaledIntervalSessionNum,Age,Age_2,Age_Years,Sex,uniqueid,AccRT)
write.csv(x = df.behavioral, file = paste0('../data/ProcessedData/','CleanData', 'SpeedAccuracy','_nSubs_',length(unique(df.trial$SubID)),'.csv'), row.names = FALSE)


#### Save out the data for DDM ####

## All subjects

# Only automatic errors & all blocks
df.DDM = df.trial %>%
  filter(ErrorType != "random")%>%
  dplyr::select(SubID,trialNum,hit,rt,intervalType,blockType,Switch,congruence,scaledRunningTime,Age,Age_2,Age_Years) %>%
  mutate(rt=rt/1000,
         response=hit,
         congruency=ifelse(congruence==1,"congruent","incongruent")) %>%
  rename(subj_idx=SubID) %>% # this threw error "Error in rename(., subj_idx = SubID) : unused argument (subj_idx = SubID)"
  dplyr::select(-hit,-congruence)

df.DDM$response = ifelse(df.DDM$response==1,1,-1)

# Save to the HSSM folder
write.csv(x = df.DDM[!is.na(df.DDM$Age), ], file = paste0('../analyses/hssm/data/','DDM_','Mixed','_AllSubs','.csv'), row.names = FALSE)

# Only automatic errors & varying only blocks
df.DDM = df.trial %>%
  filter(ErrorType != "random")%>%
  filter(blockType == "Mixed")%>%
  filter(!is.na(Switch))%>%
  dplyr::select(SubID,trialNum,hit,rt,intervalType,blockType,Switch,congruence,scaledRunningTime,Age,Age_2,Age_Years) %>%
  mutate(rt=rt/1000,
         response=hit,
         congruency=ifelse(congruence==1,"congruent","incongruent")) %>%
  rename(subj_idx=SubID) %>% # this threw error "Error in rename(., subj_idx = SubID) : unused argument (subj_idx = SubID)"
  dplyr::select(-hit,-congruence)

df.DDM$response = ifelse(df.DDM$response==1,1,-1)

# Save to the HSSM folder
write.csv(x = df.DDM[!is.na(df.DDM$Age), ], file = paste0('../analyses/hssm/data/','DDM_','Mixed_VaryingBlocks','_AllSubs','.csv'), row.names = FALSE)


#### Pre-processing summary ####
preproSummary = as.data.frame(matrix())
preproSummary$V1 = "NSubs"

# Time on task and quizzes
preproSummary$Initial = StartNParticipants

# Time on task and quizzes
preproSummary$Quizzes = ExcludeQuiz 

# Too many responses
preproSummary$TooManyResponses = ExcludeTooManyResponses 

# Accuracy
preproSummary$LowAccuracy = ExcludeAccuracy 

# Final

# Demographics
final_participants = df.trial %>% distinct(SubID, .keep_all = TRUE)


StartAgeClear = length(unique(final_participants$SubID))

# Remove rows with NaN in Age
final_participants <- final_participants %>%
  filter(!is.na(Age))

final_participants <- final_participants %>%
  filter(!is.na(Age))

preproSummary$RemoveNoAge = NExpiredAge

# Final number
preproSummary$Final = length(unique(df.trial$SubID))


# Median age
preproSummary$MedianAge = median(final_participants$Age_Years,na.rm = T)


# Gender
preproSummary$Male = length(final_participants[ which(final_participants$Sex=='Male'),]$Sex)
preproSummary$Female = length(final_participants[ which(final_participants$Sex=='Female'),]$Sex)


# Final

write.csv(preproSummary, file = "PreprocessingSummary.csv")

