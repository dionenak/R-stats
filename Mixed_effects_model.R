#Setting working directory
#setwd('C:/..)
#Setting contrasts to sum-zero for unordered, polynomial for ordered factors
options(contrasts = c("contr.sum", "contr.poly"))

#Loading the datasets
ratings<-read.csv("RatingData.csv")
age<-read.csv("AgeGroups.csv")
ratings
age

#Inspection
summary(ratings$pp_code)

## All participants gave 12 ratings each.Good!
## Now i have to merge datasets.
rat<-merge(ratings,age,by="pp_code")
## Checking the variables
summary(rat$Amount)
summary(rat$Delay)
summary(rat$Rating)
summary(age) # If i try the summary(rat) for the number of agegroups, I get different results from 
## what i want 
## NAs values
unique(is.na(rat)) # No missing values
## Center the predictors!
rat$Delayc<- scale(rat$Delay, center = TRUE, scale = FALSE)
rat$Amountc<- scale(rat$Amount, center = TRUE, scale = FALSE)


# Plotting the raw data

library(lattice) # For densityplot
with(rat, densityplot(~ Rating, groups = AgeGroup, auto.key = TRUE))
with(rat, densityplot(~ Rating | AgeGroup))
xyplot(Rating ~ Delay | AgeGroup, data = rat, type = c('r','p'), xlim = c(0, 4))
xyplot(Rating ~ Amount | AgeGroup, data = rat, type = c('r','p'), xlim = c(0,100))

# Checking whether there are higher-order relationships than linear between IVs and DV
with(rat, densityplot(~Rating | Amount))
with(rat, densityplot(~Rating | Delay))
xyplot(Rating ~ Delay, data = rat, type = c('r','p'), xlim = c(0, 4))
xyplot(Rating ~ Amount, data = rat, type = c('r','p'), xlim = c(0,100))
xyplot(Rating ~ Amount | pp_code, data = rat, type = c('p', 'r'))
xyplot(Rating ~ Delay | pp_code, data = rat, type = c('p', 'r'))

# Trying to figure out the model
## We have to see which variables could be random slopes of the 5 we are interested in: Amount, Delay,
## AgeGroups, AgeGroups:Delay, AgeGroups:Amount
with(rat, table(AgeGroup, pp_code))# Age doesn't vary between participants
with(rat,table(Delay,pp_code))# Delay does vary between participants
with(rat,table(Amount,pp_code))# Amount does vary between participants
with(rat,table(Amount,AgeGroup,pp_code)) # The interaction between Amount and age groups does not 
# vary between participants
with(rat,table(Delay,AgeGroup,pp_code)) # The interaction between Delay and age groups does not
# vary between participants

# Our model
library(lme4)
model<-lme4::lmer(Rating~AgeGroup+Delayc+Amountc+Amountc:AgeGroup+Delayc:AgeGroup+(1+(Delayc+Amountc)|pp_code),data=rat)

## I am going to use the trick to see if my random effects could be estimated
## 1. Take the data from one single "unit" (i.e., here a participant)
rat1<- rat[rat$pp_code == 'pp_1', ]
rat1
## 2.  Run an lm() model with my random effects of the lmer model
lm_check <- lm(Rating ~ 1 + Delayc+Amountc, data = rat1)
summary(lm_check) # It was possible to run the model, so in principle, I will be able to run the model
# with those random effects.
## Now i will look the summary of my model to see whether there is something wrong and that's why i
## get warning messages
summary(model)

## So to get the *number* of fixed effects in the model:
length(getME(model, "beta")) #9

## For random effects:
length(getME(model, "theta")) # 6


## To get the residual variance
length(getME(model, "sigma")) # 1

## Standardize the predictors
rat$Delays<- scale(rat$Delay, center = F, scale = TRUE)
rat$Amounts<- scale(rat$Amount, center = F, scale = TRUE)
model1<-lme4::lmer(Rating~AgeGroup+Delays+Amounts+Amounts:AgeGroup+Delays:AgeGroup+(1+(Delays+Amounts)|pp_code),data=rat)
summary(model1)

# Diagnostic Plots
densityplot(resid(model, scaled = TRUE))
library(car)
qqPlot(scale(resid(model)))
plot(model, type = c('p', 'smooth'))
with(rat, plot(Rating, fitted(model)))
with(rat, abline(lm(fitted(model) ~ Rating), col = 'red'))
## Also checking the proportion of residuals
sum(abs(resid(model1, scaled = TRUE)) > 2) / length(resid(model1))
sum(abs(resid(model1, scaled = TRUE)) > 2.5) / length(resid(model1))
sum(abs(resid(model1, scaled = TRUE)) > 3) / length(resid(model1))

## Influence statistics
infl_model1 <- influence(model1, group = "pp_code")
infIndexPlot(infl_model1)# we cannot really tell, too many participants
max(cooks.distance(infl_model1))
sum(abs(cooks.distance(infl_model1)) > 4/length(unique(rat$pp_code)))
max(dfbeta(infl_model1))#big number!
sum(abs(dfbeta(infl_model1)) > 2/sqrt(length(unique(rat$pp_code)))) 
hits <- which(abs(dfbeta(infl_model1)) > 2/sqrt(length(unique(rat$pp_code)))) 
summary(rat$pp_code[hits])

## Check the p values
Anova(model1, type = 3, test = "F")

# Follow up models: both interaction and main affect of age are significant
## Each model with their diagnostic

## Adolescents-Adults
library(afex)#for mixed() function
rat_AdolAdul <- droplevels(subset(rat, AgeGroup == "Adolescents" | AgeGroup == "Adults" ))
summary(rat_AdolAdul)
rat_AdolAdul$pp_code
## Standardize
rat_AdolAdul$Delays<- scale(rat_AdolAdul$Delay, center = T, scale = T)
rat_AdolAdul$Amounts<- scale(rat_AdolAdul$Amount, center = T, scale = T)
## Model
model_AdolAdul<-lme4::lmer(Rating~AgeGroup+Delays+Amounts+Amounts:AgeGroup+Delays:AgeGroup+(1+(Delays+Amounts)|pp_code),
data=rat_AdolAdul)
summary(model_AdolAdul)
mixed_modelAdolAdul <- mixed(Rating~AgeGroup+Delays+Amounts+Amounts:AgeGroup+Delays:AgeGroup+(1+(Delays+Amounts)|pp_code),
data=rat_AdolAdul, type = 3, method = "KR", test_intercept = TRUE)
anova(mixed_modelAdolAdul)
# Diagnostic plots
densityplot(resid(model_AdolAdul , scaled = TRUE)) # looks pretty ok
qqPlot(resid(model_AdolAdul, scaled = TRUE)) # looks pretty ok 
plot(model_AdolAdul, type = c('p', 'smooth')) # ok

## Compute the proportions of residuals for +/- 2, 2.5, 3
sum(abs(resid(model_AdolAdul , scaled = TRUE)) > 2) / length(resid(model_AdolAdul))  #0.03452381
sum(abs(resid(model_AdolAdul , scaled = TRUE)) > 2.5) / length(resid(model_AdolAdul)) # 0.01190476
sum(abs(resid(model_AdolAdul , scaled = TRUE)) > 3) / length(resid(model_AdolAdul)) # 0

## Adolescents-Children
rat_AdolChil <-droplevels(subset(rat, AgeGroup == "Adolescents" | AgeGroup == "Children" ))
summary(rat_AdolChil)
rat_AdolChil$pp_code
## Standardize
rat_AdolChil$Delays<- scale(rat_AdolChil$Delay, center = T, scale = TRUE)
rat_AdolChil$Amounts<- scale(rat_AdolChil$Amount, center = T, scale = TRUE)
## Models
model_AdolChil<-lme4::lmer(Rating~AgeGroup+Delays+Amounts+Amounts:AgeGroup+Delays:AgeGroup+(1+(Delays+Amounts)|pp_code),
data=rat_AdolChil)
summary(model_AdolChil)
mixed_modelAdolChil <- mixed(Rating~AgeGroup+Delays+Amounts+Amounts:AgeGroup+Delays:AgeGroup+(1+(Delays+Amounts)|pp_code),
data=rat_AdolChil, type = 3, method = "KR", test_intercept = TRUE)
anova(mixed_modelAdolChil)
## Diagnostic plots
densityplot(resid(model_AdolChil , scaled = TRUE)) # looks pretty ok
qqPlot(resid(model_AdolChil, scaled = TRUE)) # looks pretty ok 
plot(model_AdolChil, type = c('p', 'smooth'))#ok
# Compute the proportions of residuals for +/- 2, 2.5, 3
sum(abs(resid(model_AdolChil , scaled = TRUE)) > 2) / length(resid(model_AdolChil))  # 0.0390625
sum(abs(resid(model_AdolChil, scaled = TRUE)) > 2.5) / length(resid(model_AdolChil)) #  0.01302083
sum(abs(resid(model_AdolChil , scaled = TRUE)) > 3) / length(resid(model_AdolChil)) #  0.005208333

## Adults-Children
rat_AdulChil <-droplevels(subset(rat, AgeGroup == "Adults" | AgeGroup == "Children" ))
summary(rat_AdulChil)
rat_AdulChil$pp_code
## Standardize
rat_AdulChil$Delays<- scale(rat_AdulChil$Delay, center = T, scale = TRUE)
rat_AdulChil$Amounts<- scale(rat_AdulChil$Amount, center = T, scale = TRUE)
## Model
model_AdulChil<-lme4::lmer(Rating~AgeGroup+Delays+Amounts+Amounts:AgeGroup+Delays:AgeGroup+(1+(Delays+Amounts)|pp_code),data=rat_AdulChil)
summary(model_AdulChil)
mixed_modelAdulChil <- mixed(Rating~AgeGroup+Delays+Amounts+Amounts:AgeGroup+Delays:AgeGroup+(1+(Delays+Amounts)|pp_code),data=rat_AdulChil,
type = 3, method = "KR", test_intercept = TRUE)
anova(mixed_modelAdulChil)
## Diagnostic plots
densityplot(resid(model_AdulChil , scaled = TRUE)) # looks pretty ok
qqPlot(resid(model_AdulChil, scaled = TRUE)) # looks pretty ok (it looks like there might be some outliers, but I'll check 
# when I compute the proportions below)
plot(model_AdulChil, type = c('p', 'smooth')) # ok
## Compute the proportions of residuals for +/- 2, 2.5, 3
sum(abs(resid(model_AdulChil , scaled = TRUE)) > 2) / length(resid(model_AdulChil))  # 0.04032258
sum(abs(resid(model_AdulChil, scaled = TRUE)) > 2.5) / length(resid(model_AdulChil)) #  0.002688172
sum(abs(resid(model_AdulChil , scaled = TRUE)) > 3) / length(resid(model_AdulChil)) # 0

# Further follow up model to see each age group differently
## Adolescents
rat_Adol <- droplevels(subset(rat, AgeGroup == "Adolescents" ))
## Standardize
rat_Adol$Delays<- scale(rat_Adol$Delay, center = T, scale = T)
rat_Adol$Amounts<- scale(rat_Adol$Amount, center = T, scale = T)
## Model
model_Adol <- lme4::lmer(Rating~+Delays+Amounts+(1+(Delays+Amounts)|pp_code),data=rat_Adol,
control = lmerControl(optimizer = "bobyqa"))
summary(model_Adol)
mixed_modelAdol <- mixed(Rating~Delays+Amounts+(1+(Delays+Amounts)|pp_code),data=rat_Adol, type = 3, method = "KR",
test_intercept = TRUE,control = lmerControl(optimizer = "bobyqa"))
anova(mixed_modelAdol)
## Diagnostics
densityplot(resid(model_Adol , scaled = TRUE)) # looks pretty ok
qqPlot(resid(model_Adol, scaled = TRUE)) # looks pretty ok (it looks like there are outliers. I'll check when I
#compute the proportions)
plot(model_Adol, type = c('p', 'smooth')) # ok
## Compute the proportions of residuals for +/- 2, 2.5, 3
sum(abs(resid(model_Adol , scaled = TRUE)) > 2) / length(resid(model_Adol))  # 0.04166667
sum(abs(resid(model_Adol, scaled = TRUE)) > 2.5) / length(resid(model_Adol)) #  0.01851852
sum(abs(resid(model_Adol , scaled = TRUE)) > 3) / length(resid(model_Adol)) # 0

## Adults
rat_Adul <- droplevels(subset(rat, AgeGroup == "Adults" ))
summary(rat_Adul)
rat_Adul$pp_code

## Standardize
rat_Adul$Delays<- scale(rat_Adul$Delay, center = T, scale = T)
rat_Adul$Amounts<- scale(rat_Adul$Amount, center = T, scale = T)
## Model
model_Adul <- lme4::lmer(Rating~+Delays+Amounts+(1+(Delays+Amounts)|pp_code),data=rat_Adul,
control = lmerControl(optimizer = "bobyqa"))
summary(model_Adul)
mixed_modelAdul <- mixed(Rating~Delays+Amounts+(1+(Delays+Amounts)|pp_code),data=rat_Adul, type = 3, method = "KR",
test_intercept = TRUE,control = lmerControl(optimizer = "bobyqa"))
anova(mixed_modelAdul)
## Diagnostics
densityplot(resid(model_Adul , scaled = TRUE)) # looks ok
qqPlot(resid(model_Adul, scaled = TRUE)) # looks pretty ok 
plot(model_Adul, type = c('p', 'smooth')) # not that good, but ok
## Compute the proportions of residuals for +/- 2, 2.5, 3
sum(abs(resid(model_Adul , scaled = TRUE)) > 2) / length(resid(model_Adul))  #  0.03186275
sum(abs(resid(model_Adul, scaled = TRUE)) > 2.5) / length(resid(model_Adul)) #  0
sum(abs(resid(model_Adul , scaled = TRUE)) > 3) / length(resid(model_Adul)) # 0

## Children
rat_Chil <- droplevels(subset(rat, AgeGroup == "Children" ))
summary(rat_Chil$pp_code)

## Standardize
rat_Chil$Delays<- scale(rat_Chil$Delay, center = T, scale = T)
rat_Chil$Amounts<- scale(rat_Chil$Amount, center = T, scale = T)
## Model
model_Chil <- lme4::lmer(Rating~Delays+Amounts+(1+(Delays+Amounts)|pp_code),data=rat_Chil,
control = lmerControl(optimizer = "bobyqa"))#singularity
model_Chil_norandcor <- lme4::lmer(Rating~Delays+Amounts+((1+(Delays+Amounts))||pp_code),data=rat_Chil,
control = lmerControl(optimizer = "bobyqa"))#remove random correlations but again singularity
model_Chil_default <- lme4::lmer(Rating~Delays+Amounts+((1+(Delays+Amounts))||pp_code),data=rat_Chil)
# Remove random correlations,again singularity


library(dfoptim)
allFit_model_Chil <- allFit(model_Chil_default)
summ_allFit_model_Chil <- summary(allFit_model_Chil)
summ_allFit_model_Chil$fixef
summ_allFit_model_Chil$sdcor
## The results are pretty similar, false positive!
summary(model_Chil)
mixed_modelChil <- mixed(Rating~Delays+Amounts+(1+(Delays+Amounts)|pp_code),data=rat_Chil, type = 3, method = "KR",
test_intercept = TRUE,control = lmerControl(optimizer = "bobyqa"))
anova(mixed_modelChil)

## Diagnostic
densityplot(resid(model_Chil , scaled = TRUE)) # looks pretty ok
qqPlot(resid(model_Chil, scaled = TRUE)) # looks pretty ok (it looks like there are outliers. I'll check when I
#compute the proportions)
plot(model_Chil, type = c('p', 'smooth')) # ok
## Compute the proportions of residuals for +/- 2, 2.5, 3
sum(abs(resid(model_Chil , scaled = TRUE)) > 2) / length(resid(model_Chil))  # 0.04761905
sum(abs(resid(model_Chil, scaled = TRUE)) > 2.5) / length(resid(model_Chil)) #  0.005952381
sum(abs(resid(model_Chil , scaled = TRUE)) > 3) / length(resid(model_Chil)) # 0
 
## Result-visuals
with(rat, interaction.plot(Delay, AgeGroup, Rating))
library(effects)
plot(effect("AgeGroup:Delays", model1), multiline = TRUE, xlab="Time of delivery (standardized)")
plot(effect("AgeGroup", model1))
scatterplot(Rating ~ Delay, smooth = TRUE, boxplots =FALSE, data = rat)
scatterplot(Rating ~ Amount, smooth = TRUE, boxplots =FALSE, data = rat)
## We will plot the random effects but we won't put it in our report
dotplot(ranef(model1, condVar = TRUE))
qqmath(ranef(model1, condVar = TRUE))


## Citation
citation()
citation("afex")
citation("effects")
citation("dfoptim")
citation("lme4")
citation("car")
citation("lattice")
citation("pbkrtest")
