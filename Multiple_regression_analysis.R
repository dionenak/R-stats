#Setting working directory
#setwd("C:\\..")
#Load packages
library(pastecs) #For stat.desc#
citation("pastecs")
library(lattice) #For density plot#
citation("lattice")
library(ggplot2)#For other plots (e.g scatterplots for correlations)
citation("ggplot2")
library(gridExtra)
citation("gridExtra")#For grid.arrange()
library(mvnmle)
citation("mvnmle")
library(BaylorEdPsych) # For MCAR test
citation("BaylorEdPsych")
library(ltm)# For correlations and p-values (rcor.test)
citation("ltm")
library(gridExtra)# For grid.arrange() command
citation("gridExtra")
library(ggpubr)# For ggerrorplot()
citation("ggpubr")
library(VIM) #For aggr() function
citation("VIM")
library(car)
citation("car")
library(jtools)# For sim_slopes
citation("jtools")
library(interactions)
citation("interactions")

#Loading our dataset
social<-read.csv2("midterm2_2018.csv")
#Rename gender column and turn the variable into a factor
colnames(social)[1] <- "gender"
social$gender<-as.factor(social$gender)

# PART A: Preliminary analyses #
#Inspection of our dataset
##We can detect if there are impossible values by looking at max and min, given our report.
summary(social)

##To find the impossible values in variable sPerformance
sum(abs(social$sPerform)>25)
which(abs(social$sPerform)>25) ##In which row?
social <- social[-8,]  ##Removing the row
## Copy the column of sIQ and paste it in another variable
Col_IQ<-social$sIQ
Col_IQ<-na.omit(Col_IQ) ## Remove NAs
which(abs(Col_IQ)>5) ## Find which rows have impossible values
social<-social[-181,]  ## Delete this row in the main dataframe
summary(social)
social ##We don't find impossible values anymore


aggr(social, numbers=T) ##Patterns and prevalence of missing values

##For MCAR test
skillsmiss <- LittleMCAR(social)
skillsmiss$chi.square
skillsmiss$df
skillsmiss$p.value

#Univariate distributions
##Skewness and kurtosis
stat.desc(social, norm=T)
densityplot(social$sSkills,xlab="Social skills")
densityplot(social$sPerform,xlab="Social performance")
## Plots
densityplot(social$sComplex,xlab="Social complexity")
densityplot(social$sIQ, xlab="Social intelligence")
with(social, hist(sSkills))
with(social, hist(sPerform))
with(social, hist(sComplex))
with(social, hist(sIQ))
#Inspect visually for outliers
with(social, boxplot(sSkills, ylab='Social Skills'))
with(social, boxplot(sPerform, ylab='Social Performance'))
with(social, boxplot(sComplex, ylab='Social Complexity'))
with(social, boxplot(sIQ, ylab='Social Intelligence'))
##In order to find arithmetically how many the outliers are we have to standardize our continuous 
##variables and then see how many values of each variable is larger that 3SD
stand<-scale(social[,c(2,3,4,5)])
outliers <- colSums(abs(stand)>=3, na.rm = T)
outliers

#Bivariate associations among the continuous variables
##Pearsons correlation with listwise deletion
complete.cases(social)
social[!complete.cases(social),]
social1<-na.omit(social) #Deleted rows with unknown values
Corr<-rcor.test(social1[,c(2,3,4,5)]) #Correlation
Corr

##Plots for the correlations with fitted("loess") 
skills_perform <- ggplot(social1, aes(x = sSkills, y = sPerform))
skills_perform<-skills_perform + geom_point()+ geom_smooth(method = "loess")+
 xlab("Social Skills") + ylab("Social Performance") 
 
skills_Complex <- ggplot(social1, aes(x = sSkills, y = sComplex))
skills_Complex <-skills_Complex + geom_point()+ geom_smooth(method = "loess")+
 xlab("Social Skills") + ylab("Social Complexity") 
  
skills_IQ <- ggplot(social1, aes(x = sSkills, y = sIQ))
skills_IQ<-skills_IQ + geom_point()+ geom_smooth(method = "loess")+
 xlab("Social Skills") + ylab("Social Intelligence") 
  
perform_complex <- ggplot(social1, aes(x = sPerform, y = sComplex))
perform_complex<-perform_complex  + geom_point()+ geom_smooth(method = "loess")+
 xlab("Social Performance") + ylab("Social Complexity") 

perform_IQ <- ggplot(social1, aes(x = sPerform, y = sIQ))
perform_IQ<-perform_IQ  + geom_point()+ geom_smooth(method = "loess")+
 xlab("Social performance") + ylab("Social Intelligence") 
 
Complex_IQ <- ggplot(social1, aes(x = sComplex, y = sIQ))
Complex_IQ<-Complex_IQ  + geom_point()+ geom_smooth(method = "loess")+
 xlab("Social complexity") + ylab("Social Intelligence") 
  
grid.arrange(skills_perform,
skills_Complex ,
skills_IQ,perform_complex,
perform_IQ,
Complex_IQ, ncol = 3)

#Perform four t tests for each continuous variables and gender
t.test(social1$sSkills ~ social1$gender)
by(social1$sSkills, social1$gender, stat.desc)
##For 95% CIs
3.21- 0.087  #Mean-/+ CI.mean.0.95 for gender=0
3.21+ 0.087
3.43-0.076   #Mean-/+ CI.mean.0.95 for gender=1
3.43+0.076

t.test(social1$sPerform ~ social1$gender)
by(social1$sPerform, social1$gender, stat.desc)
#For 95% CIs
12.95- 0.326
12.95+ 0.326
15.35-0.330
15.35+0.330

t.test(social1$sComplex~ social1$gender)
by(social1$sComplex, social1$gender, stat.desc)
#for 95% CIs
2.84-0.100
2.84+0.100
2.68-0.091
2.68+0.091

t.test(social1$sIQ ~ social1$gender)
by(social1$sComplex, social1$gender, stat.desc)
##Non significant result but i have to report M and CIs
3.30-0.076
3.30+0.076
3.30-0.092
3.30+0.092

##Plots for gender differences for our four variables
q<-ggerrorplot(social1, x = "gender", y = "sSkills", 
            desc_stat = "mean_se",
               color= "gender" ,
             palette = "jco", ylab="social skills", xlab= "gender(0 for males, 1 for females)")
w<-ggerrorplot(social1, x = "gender", y = "sPerform", 
            desc_stat = "mean_se",
               color= "gender" ,
             palette = "jco", ylab="social performance", xlab= "gender(0 for males, 1 for females)")

e<-ggerrorplot(social1, x = "gender", y = "sComplex", 
            desc_stat = "mean_se",
               color= "gender" ,
             palette = "jco", ylab="Complexity of social situation", xlab= "gender(0 for males, 1 for females)")

r<-ggerrorplot(social1, x = "gender", y = "sIQ", 
            desc_stat = "mean_se",
               color= "gender" ,
             palette = "jco", ylab="social intelligence", xlab= "gender(0 for males, 1 for females)")
grid.arrange(q,
w ,
e,r,
 ncol = 2)

# B PART : Testing moderation and assumptions #
##Centering all predictors
social1$sSkills_c<-c(scale(social1$sSkills, scale = F))
social1$sComplex_c<-c(scale(social1$sComplex, scale = F))
social1$sIQ_c<-c(scale(social1$sIQ,scale=F))


#Multiple Regression analysis
model <- lm(sPerform ~  sComplex_c +sSkills_c + sIQ_c+ gender+ sSkills_c:sComplex_c+ sSkills_c:sComplex_c:sIQ_c+sSkills_c:sComplex_c:gender, social1)
summary(model)

#Assumptions

##1.Normality of residuals
social1$res_model <- rstandard(model)
plot1<-densityplot(social1$res_model, xlab="Residuals")
plot1
plot2<-qqPlot(social1$res_model)
plot2
##compute the proportion of residuals
length(which(abs(social1$res_model) > 2)) / length(social1$res_model)
length(which(abs(social1$res_model) > 2.5)) / length(social1$res_model)
length(which(abs(social1$res_model) > 3)) / length(social1$res_model)

##2.Checking linearity and homo/heteroscedasticity
plot(model, which = 1)

##3. Cook's distance
plot(model, which = 4)

##4. Multicollinearity
vif(model)

##Calculation the unstandardized coefficients of our model
b0 = as.numeric(model$coefficients[1]) 
b0
b1 = as.numeric(model$coefficients[2]) 
b1
b2 = as.numeric(model$coefficients[3]) 
b2
b3 = as.numeric(model$coefficients[4])
b3
b4 = as.numeric(model$coefficients[5])
b4 
b5 = as.numeric(model$coefficients[6])
b5 
b6 = as.numeric(model$coefficients[7]) 
b6
b7 = as.numeric(model$coefficients[8])
b7

##Find standardized beta coefficients, first we have to standardize the continuous IVs and DV and then calculate the multiple regression
social1$sSkills_s<-c(scale(social1$sSkills))
social1$sComplex_s<-c(scale(social1$sComplex))
social1$sIQ_s<-c(scale(social1$sIQ))
social1$sPerform_s<-c(scale(social1$sPerform))
model1 <- lm(sPerform_s ~  sComplex_s +sSkills_s + sIQ_s+ gender+ sSkills_s:sComplex_s+ sSkills_s:sComplex_s:sIQ_c + sSkills_s:sComplex_s:gender, social1)
summary(model1)
b0 = as.numeric(model1$coefficients[1]) 
b0
b1 = as.numeric(model1$coefficients[2]) 
b1
b2 = as.numeric(model1$coefficients[3]) 
b2
b3 = as.numeric(model1$coefficients[4])
b3
b4 = as.numeric(model1$coefficients[5])
b4 
b5 = as.numeric(model1$coefficients[6])
b5 
b6 = as.numeric(model1$coefficients[7]) 
b6
b7 = as.numeric(model1$coefficients[8])
b7

# Again multiple regression model but only with the significant interactions with the whole sample
model2<-lm(sPerform~sComplex_c*sSkills_c, social1)
b0 = as.numeric(model2$coefficients[1]) 
b0
b1 = as.numeric(model2$coefficients[2]) 
b1
b2 = as.numeric(model2$coefficients[3]) 
b2
b3 = as.numeric(model2$coefficients[4])
b3
##Simple intercept for entire sample
b0+b2
##Simple slop for entire sample
b1+b3
##In order to plot the simple slopes for three levels of social skills calculate three meaningful
##values of social skills
mean(social1$sSkills_c)
sd(social1$sSkills_c)
H.sskills <-mean(social1$sSkills_c) + sd(social1$sSkills_c) # +1 SD
M.sskills<-mean(social1$sSkills_c) #0 SD
L.sskills<-mean(social1$sSkills_c) -sd(social1$sSkills_c) # -1 SD
H.sskills
M.sskills
L.sskills
##The slopes and intercepts for the three levels of social skills
Hint <-b0 + b2*H.sskills
Hslp<-b1 + b3*H.sskills
Mint <-b0 + b2*M.sskills
Mslp<-b1 + b3*M.sskills
Lint <-b0 + b2*L.sskills
Lslp<-b1 + b3*L.sskills
Hint
Hslp
Mint
Mslp
Lint
Lslp
## Finding 2 points for each level in order to plot the simple slops
###points for high level of social skills
HsskHp <- Hint + Hslp * (mean(social1$sComplex_c) + ( sd(social1$sComplex_c))) # based on  SD of complexity
HsskLp <- Hint + Hslp * (mean(social1$sComplex_c) + (- sd(social1$sComplex_c))) # based on - SD of present
c(HsskLp,HsskHp)

###Points for medium level of social skills
MsskHp <- Mint + Mslp * (mean(social1$sComplex_c) + ( sd(social1$sComplex_c))) # based on  SD of complexity
MsskLp <- Mint + Mslp * (mean(social1$sComplex_c) + (- sd(social1$sComplex_c))) # based on - SD of present
c(MsskLp,MsskHp)

###Points for low level of social skills
LsskHp <- Lint + Lslp * (mean(social1$sComplex_c) + ( sd(social1$sComplex_c))) # based on  SD of complexity
LsskLp <- Lint + Lslp * (mean(social1$sComplex_c) + (- sd(social1$sComplex_c))) # based on - SD of present
c(LsskLp,LsskHp)


##Computing the standard errors for simple slopes
###Obtain variances and covariances
vcov_c<-vcov(model2)
vcov_c
var1 = vcov_c[2,2]
var3 = vcov_c[4,4]
cov13 = vcov_c[4,2]
var1
var3
cov13
###Calculating p-values, t-values and SE
Hslp_se <- sqrt(var1 + (2*H.sskills*cov13) + ((H.sskills**2) * var3)) # SE for simple slope for high level of social skills
Hslp_t <- Hslp / Hslp_se                                      # t-value for slope for high level of social skills
Hslp_p <- 2*pt(-abs(Hslp_t),(length(social1$sComplex)-4))      # p-value for slope for high level of social skills
c(Hslp_se,Hslp_t, Hslp_p)

Mslp_se <- sqrt(var1 + (2*M.sskills*cov13) + ((M.sskills**2) * var3)) # SE for simple slope for medium level of social skills
Mslp_t <- Mslp / Mslp_se                                      # t-value for slope for medium level of social skills
Mslp_p <- 2*pt(-abs(Mslp_t),(length(social1$sComplex)-4))      # p-value for slope for medium level of social skills
c(Mslp_se,Mslp_t, Mslp_p)

Lslp_se <- sqrt(var1 + (2*L.sskills*cov13) + ((L.sskills**2) * var3)) # SE for simple slope for medium level of social skills
Lslp_t <- Lslp / Lslp_se                                      # t-value for slope for medium level of social skills
Lslp_p <- 2*pt(-abs(Lslp_t),(length(social1$sComplex)-4))      # p-value for slope for medium level of social skills
c(Lslp_se,Lslp_t, Lslp_p)


#Simple slopes
sim_slopes(model2, pred= sComplex_c, modx= sSkills_c, johnson_neyman= FALSE)
interact_plot(model2, pred= "sComplex_c", modx= "sSkills_c",  interval = T, int.width= 0.90, x.label= "Complexity of social situations", y.label= "Social performance", legend.main= "Social skills")