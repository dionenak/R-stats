##Setting our working directory
##setwd("C:\\..")
##Loading our data
df<-read.csv2("..", header=T)

#Part A - Inspection
##Let's take a look at the first rows of our dataframe
head(df)
##Skewness and kurtosis
library("pastecs")# For descriptive statistics
citation("pastecs")
library("lattice")# For density plots
citation("lattice")
##Check the continuous variables
##First from descriptive statistics
stat.desc(df[,2:9], basic = FALSE, desc = FALSE, norm = TRUE)
## Secondly using visualization
densityplot(df$educate)
densityplot(df$income)
densityplot(df$involve)
densityplot(df$school)
densityplot(df$teach)
densityplot(df$accept)
densityplot(df$achieve)
densityplot(df$CITO)
## Although, at first sight, plots look ok, descriptive statistics are clear: only educate is normally distributed
## Look for univariate outliers
standardized <- scale(df[,c(2:9)]) # Standardize variables
standardized<-as.data.frame(standardized)
outliers <- colSums(abs(standardized)>3, na.rm = T) # Count number of cases above/below 3
## Let's see the number of outliers
outliers  # We have 9 outliers in total
## Missing values
## Let's see the total number for each variable
summary(df)
## Now let's see the patterns of missingness
library(VIM) # For the patterns of missingness
citation("VIM")
missing <- aggr(df)
summary(missing)
## For checking MCAR
library("BaylorEdPsych")# For MCAR test
library("mvnmle")# For MCAR test
citation("BaylorEdPsych")
citation("mvnmle")
MCAR <- LittleMCAR(df)
MCAR$chi.square
MCAR$df
MCAR$p.value # Our data are not MCAR!
## Test multivariate normality and outliers
library(MVN)
citation("MVN")
## Create subset with complete cases...required for MVN
df_comp <- df[complete.cases(df),]
names(df_comp)
MVdf <- mvn(df_comp[, c(2:9)], mvnTest = "royston", multivariatePlot = "qq",multivariateOutlierMethod = "adj", showOutliers = T, showNewData = T)
MVdf
## Multivariate NON-normal; no outliers; Also, we see that for univariate distributions, only educate variable satisfies the requirements for being normal distributed.
## Now let's check whether the correlations differ depending on listwise or pairwise deletion
library("psych")
citation("psych")
corr.test(df[2:9], df[2:9], use = "complete")
corr.test(df[2:9], df[2:9])# Default pairwise deletion
## No, they are quite similar

#PART B - Let's see the model now
## First the measurement model
library(lavaan)
citation("lavaan")
mdl1 <- ' SQ=~ 1*school + teach + accept      
HE =~ 1*educate + income + involve
#variances
educate~~educate
income~~income
involve~~involve
teach~~teach
accept~~ accept
school~~ school
SQ~~SQ
HE~~HE
#Intercepts
educate~1
income~1
involve~1
teach~1
accept~1
school~1

#covariance
HE~~SQ
'
fit1<-lavaan(mdl1,data=df, missing = "fiml.x", estimator='MLR',test='Yuan-Bentler',se= "robust.huber.white")
summary(fit1, fit.measures=T, standardized=T)
fitMeasures(fit1)
## SEM model
mdl2<-' #regression
achieve~SQ+HE
CITO~achieve
#latent variables
SQ=~ 1*school + teach + accept      
HE =~ 1*educate + income + involve
#variances
educate~~educate
income~~income
involve~~involve
teach~~teach
accept~~ accept
school~~ school
SQ~~SQ
HE~~HE
CITO~~CITO
achieve~~achieve

#covariance
HE~~SQ
#intercepts
educate~1
income~1
involve~1
teach~1
accept~1
school~1
achieve~1
CITO~1
'
fit2<-lavaan(mdl2,data=df, missing = "fiml.x", estimator='MLR',test='Yuan-Bentler',se= "robust.huber.white")
summary(fit2,fit.measures=T, standardized=T, rsquare=T)
## Let's look for misspecifications
summary(fit2, fit.measures=T, modindices = T) 
mod_ind <- modificationindices(fit2)
## Which are the twenty relationships with the higher mi?
head(mod_ind[order(mod_ind$mi, decreasing=TRUE), ], 20)
# Let's visualize our model
library(semPlot)# For semPaths plot
citation("semPlot")
semPaths(fit2, layout = "tree", rotation = 2, 
         intercepts = F, residuals = F,  nCharNodes = 10,
          edge.color = "black",sizeMan=10, fade=F, esize=2,asize=2, edge.label.cex=0.5, normalize=T)

## The improved SEM model with CITO~SQ and CITO~HE
mdl3<-' #regression
achieve~SQ+HE
CITO~achieve
CITO~SQ
CITO~HE
#latent variables
SQ=~ 1*school + teach + accept      
HE =~ 1*educate + income + involve
#variances
educate~~educate
income~~income
involve~~involve
teach~~teach
accept~~ accept
school~~ school
SQ~~SQ
HE~~HE
CITO~~CITO
achieve~~achieve

#covariance
HE~~SQ
#intercepts
educate~1
income~1
involve~1
teach~1
accept~1
school~1
achieve~1
CITO~1
'
fit3<-lavaan(mdl3,data=df, missing = "fiml.x", estimator='MLR',test='Yuan-Bentler',se= "robust.huber.white" )
summary(fit3,fit.measures=T, standardized=T,rsquare=T)

## Again visualization of our SEM model
semPaths(fit3, what= "col", "std", layout = "tree", rotation = 2, 
         intercepts = F, residuals = F, curve = 2, nCharNodes = 0,
         edge.label.cex = 1, edge.color = "black", sizeMan = 10, nDigits = 3)

