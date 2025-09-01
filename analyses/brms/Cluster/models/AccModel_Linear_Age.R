library("brms")
library("MASS")

# set seed
set.seed(42) 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Import the data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
df.trial= read.csv("../../../../data/ProcessedData/CleanDataSpeedAccuracy_nSubs_245.csv")

df.trial$hit = as.numeric(df.trial$hit)

# Change the name of the congruency variable
df.trial$Congruency = df.trial$type

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Re-scale the data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Re-scale the age variables so that they are not very small
df.trial$Age = df.trial$Age * 100
df.trial$Age_2 = df.trial$Age_2 * 100

# Center the continuous variables 
df.trial$scaledIntervalLength = scale(df.trial$scaledIntervalLength, scale= FALSE, center = TRUE)
df.trial$scaledIntervalSessionNum = scale(df.trial$scaledIntervalSessionNum, scale= FALSE, center = TRUE)
df.trial$scaledRunningTime = scale(df.trial$scaledRunningTime, scale= FALSE, center = TRUE)

# Contrast code the categorical variables
df.trial$Congruency = ordered(df.trial$type, levels = c("congruent", "incongruent"))
contrasts(df.trial$Congruency) = contr.sdif(2)
colnames(attr(df.trial$Congruency, "contrasts")) =  c("inc_min_con")

df.trial$blockType = ordered(df.trial$blockType, levels = c("Mixed","Fixed"))
contrasts(df.trial$blockType) = contr.sdif(2)
colnames(attr(df.trial$blockType, "contrasts")) =  c("Mixed_min_Fixed")

df.trial$intervalType = ordered(df.trial$intervalType, levels = c("Speed","Accuracy"))
contrasts(df.trial$intervalType) = contr.sdif(2)
colnames(attr(df.trial$intervalType, "contrasts")) =  c("Speed_min_Accuracy")

df.trial$previntervalType = ordered(df.trial$previntervalType, levels = c("Speed","Accuracy"))
contrasts(df.trial$previntervalType) = contr.sdif(2)
colnames(attr(df.trial$previntervalType, "contrasts")) =  c("Speed_min_Accuracy")

df.trial$Switch = ordered(df.trial$Switch, levels = c("Switch","Repeat"))
contrasts(df.trial$Switch) = contr.sdif(2)
colnames(attr(df.trial$Switch, "contrasts")) =  c("Switch_min_Repeat")

df.trial$intervalNumHalf = ifelse(df.trial$intervalNum>15,"Second","First")
df.trial$intervalNumHalf = ordered(df.trial$intervalNumHalf, levels = c("Second","First"))
contrasts(df.trial$intervalNumHalf) = contr.sdif(2)
colnames(attr(df.trial$intervalNumHalf, "contrasts")) =  c("Second_min_First")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Fit the accuracy model
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
model.Acc = brm(hit ~ Congruency + intervalType*blockType*Age + scaledIntervalSessionNum + scaledIntervalLength + scaledRunningTime + (Congruency + intervalType * blockType|uniqueid),
                                         data=df.trial,
                                         family="bernoulli",
                                         #prior = prior,
                                         iter  = 20000,
                                         warmup = 19000,
                                         save_pars=save_pars(all=TRUE),
                                         cores = 4,
                                         sample_prior = TRUE,
                                         control = list(adapt_delta = 0.99))  # Increase adapt_delta
saveRDS(model.Acc,file="../output/model_linear_age.Acc.rds")
