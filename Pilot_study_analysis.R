#Loading the libraries
library(lattice)
library(ggplot2)
library(pastecs)
library(foreign)
library(Hmisc)
library(tidyr)
library(dplyr)
library(ltm)
library(pwr)
library(reshape2)
library(MBESS)

#Loading our dataset
mydata<-read.csv("pilotdata_long.csv")


#Inspect our dataset
stat.desc(mydata)
#Remove outliers and inspect again our dataset
##In order to perform our tests, we have to remove the two unknown values in participants' scores. We
##didn't exclude the missing values in "gender" variable, because this variable is not relevant for 
##our statistics analyses.
mydata2<-mydata %>% drop_na(score)
stat.desc(mydata2$age, desc=T, norm=T, p=0.95)
stat.desc(mydata2$score, desc=T, norm=T, p=0.95)



## Outliers and densityplots of scores
standardized <- scale(mydata2[,c(5)]) 
outliers <- colSums(abs(standardized)>=3) 
outliers

dplot <- qplot(score, data = mydata2, geom = "density")
dplot<-dplot+labs(title="Weight density curve",x="Scores in statistics", y = "Density")
dplot
dplot_time <- qplot(mydata2$score, data = mydata2, geom = "density", color = time)
dplot_time<-dplot_time+labs(title="Weight density curve",x="Scores in statistics", y = "Density")
dplot_time

#Correlation
## A biserial correlation was computed between participants' scores and the time of the day that 
##tasks were performed, either evening or morning, and two Pearson correlations between participants'
##age and scores, one for morning scores and a second one for evening scores
Time_numb<-as.numeric(mydata2$time)
cor.test(Time_numb,mydata2$score)

##Split the data between morning and evening measures and then see correlations between age and scores.
data <- mydata2[c(3,5)]
Time_morn<-data %>% slice(1:19)
Time_even<-data %>% slice(20:38)
cor.test(Time_even$score,Time_even$age)
cor.test(Time_morn$score,Time_morn$age)


#T-test
##A paired sample t-test examined the mean level differences on participants' scores as a function
##of time of the day. We used a paired t-test because same participants were examined in both
##evening and morning, so they belong to one sample.
t.test(Time_even$score,Time_morn$score, paired=T)
by(mydata2$score, mydata2$time, stat.desc)
leven<-46.438437 -13.384063 #Evening mean score= 46.438437 Evening CI.mean.0.95=13.384063
heven<-46.438437 +13.384063
lmorn<-32.3347368-10.6336596 #Morning mean score= 46.438437 Morning CI.mean.0.95=13.384063
hmorn<-32.3347368+10.6336596 

leven
heven
lmorn
hmorn


#Standard effect paired and independent
##We calculated the standardized effect size, delta hat, for the difference of score means for
##morning and evening in that pilot study. Even though, it is a within-subject study, we calculated
##delta hats for both a paired sample and independent sample. 

##Independent samples
smd( Mean.1= 46.438437,Mean.2=32.3347368,s.1=27.768654,s.2=22.0622406, n.1=19, n.2=19,Unbiased=T)

##Paired samples
t.to.g.rm <- function(t, n) {
t * (1/n)^0.5
}
t.to.g.unb.rm <- function(t, n) {
g <- t.to.g.rm(t, n)
g * (n - 3)/(n - 2.25)
}

t.to.g.rm(3.1881,19)

#Power
##Three power analyses examined the number of participants needed to have eighty, ninety and 
##ninety-five percent of power to detect the improvement in people's statistical skills in the
##evenings compared to their performance during the morning, using different effect sizes and 
##assuming different study designs about the samples.

##Specifically, in the first power analysis, we assumed that the pilot study has a
##between-subjects design, using delta_IG=.55 and two-samples t-test analysis, at two-sided 5% 
##significant level, with 80%, 90% and 95% power.

pwr.t.test(d=0.55, sig.level=0.05, power=0.8, alternative="two.sided",type="two.sample")
pwr.t.test(d=0.55, sig.level=0.05, power=0.9,type="two.sample",alternative="two.sided")
pwr.t.test(d=0.55, sig.level=0.05,type="two.sample",power=0.95,alternative="two.sided")

#We computed the power for sample sizes ranging from 2 to 200 per group
samplesizes=2:200

pr<-pwr.t.test(d=.55,sig.level=0.05,n=samplesizes,alternative="two.sided",type="two.sample")$power
plot(samplesizes,pr,ylim=c(0,1),ylab='Statistical power', xlab='Sample size per group')
grid()


##In the second power analysis, we assumed that the pilot study has a within-subjects design,
##using delta_IG=.55 and two-samples t-test analysis, at two sided 5% significant level, with
## 80%, 90% and 95% power.


pwr.t.test(d=0.55, sig.level=0.05, power=0.8, alternative="two.sided",type="paired")
pwr.t.test(d=0.55, sig.level=0.05, power=0.9,type="paired",alternative="two.sided")
pwr.t.test(d=0.55, sig.level=0.05,type="paired",power=0.95,alternative="two.sided")



##we computed the power for sample sizes ranging from 2 to 200 per group, for these conditions
pr2<-pwr.t.test(d=.55,sig.level=0.05,n=samplesizes,alternative="two.sided",type="paired")$power
plot(samplesizes,pr2,ylim=c(0,1),ylab='Statistical power', xlab='Sample size (pairs)')
grid()

## In the last power analysis performed, we assumed that the pilot study has a within-subjects 
##design (paired t-test), using effect size for paired t-test, delta_RM=.73, at two-sided 5% 
##significance level, with with 80%, 90% and 95% power. 

pwr.t.test(d=0.73, sig.level=0.05, power=0.8, alternative="two.sided",type="paired")
pwr.t.test(d=0.73, sig.level=0.05, power=0.9,type="paired",alternative="two.sided")
pwr.t.test(d=0.73, sig.level=0.05,type="paired",power=0.95,alternative="two.sided")

#we computed the power for sample sizes ranging from 2 to 200 per group, for these conditions
pr3<-pwr.t.test(d=.73,sig.level=0.05,n=samplesizes,alternative="two.sided",type="paired")$power
plot(samplesizes,pr3,ylim=c(0,1),ylab='Statistical power', xlab='Sample size(pairs)')

grid()

