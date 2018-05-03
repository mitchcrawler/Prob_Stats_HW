setwd("/Users/bombus/Documents/jupyter/Week8")
library(mosaic)
library(tibble)

##Question 1.

second = read.csv(file = "second.csv", header = TRUE)
second = cbind(second, 1/sqrt(second$Mortality))
second$number = seq(1:10)
attach(second)
plot(second$Mortality ~ second$Year)

second.lm.l = lm(Mortality ~ Year)

second.lm = lm(Mortality ~ Year)
summary(second.lm.l)

predict(second.lm,data.frame(Year = c(2010)))

###Predicted value at year 2010= 1.35

plot(second$Year, second$Mortality, pch=16, xlim=c(1960, 2010), ylim = c(0,30))
plot(second.lm)

newdata=data.frame(Year=2010)
predict(second.lm, newdata, se.fit = TRUE, interval = "prediction")

detach(second)

mplot(second.lm, which = 1, system = "ggplot2") ##This indicates problem with residuals
mplot(second.lm, which = 2, system = "ggplot2") ##Linearity looks ok
mplot(second.lm, which = 3, system = "ggplot2") ##Variance Problem
mplot(second.lm, which = 4, system = "ggplot2") ##Large effect of outlier

library(broom) 
augment(second.lm) %>%
  filter(.cooksd > 0.5)

###A good tranformation would be 1/sqrt of vals, that should take care of the
###non normal problem

##Question 2

hacker = read.csv(file = "HackerRank-Developer-Survey-2018-Numeric.csv")
##A.
logreg <- glm(hacker$q3Gender ~ hacker$q1AgeBeginCoding, family = "binomial", data = hacker) 
msummary(logreg)

##B.
MLRE <- lm(hacker$q8JobLevel ~ hacker$q3Gender + hacker$q1AgeBeginCoding, data = RailTrail)
msummary(MLRE)

##It does in some specific age ranges (especially early) and in females not in males 

##Question 3

Recovery = c(.53, .36, .20, -.37, -.60, -.64, -.68, -1.27, .73, .31, .03, -.29, -.56, -.96, -1.61,
             -.78, -.86, -1.35, -1.48, -1.52, -2.04, -2.83)
treatment = c(rep("control",8), rep("surgery",7), rep("acupunture",7))
data = data.frame(Recovery, treatment)

res.aov <- aov(Recovery ~ treatment, data = data)
summary(res.aov)

##Significant - but where is it significant

TukeyHSD(res.aov)

##Control VS acupunture highest difference
##surgery vs acupunture second difference
##no difference between surgery and control
