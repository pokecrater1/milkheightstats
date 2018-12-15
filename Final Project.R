stats <- read.csv("Heights of UT Students clean.csv")
hist(stats$Height..inches., main = "Height Distribution of UT Class 2020 Students",
     xlab = 'Height', col = 'red', xlim=c(40, 80), breaks = 10)
mean(stats$Height..inches.)
sd(stats$Height..inches.)

boxplot(stats$Height..inches. ~ stats$Sex, main = "Heights of Sexes",
        xlab = "Sex", ylab = "Heights")
males <- stats$Height..inches.[stats$Sex=='Male']
females <- stats$Height..inches.[stats$Sex=='Female']
length(males)
length(females)
summary(males)
summary(females)


plot(stats$Cups.of.milk.drank.weekly, stats$Height..inches., xlab = 
       'Cups of milk', ylab = 'Height', main = 'Milk drank vs Height')

hist(stats$Cups.of.milk.drank.weekly, xlim=c(0,25), breaks=10, xlab =
       'cups of milk', main = 'Cups of milk drank by UT class 2020')
summary(stats$Cups.of.milk.drank.weekly)

#MUST CENTER EXPLANATORY VARIABLE BEFORE GLM TEST
stats$milk_c <- stats$Cups.of.milk.drank.weekly-mean(stats$Cups.of.milk.drank.weekly)

mr <- lm(Height..inches. ~ Sex * milk_c, data = stats)
#confirm assumptions of equal variance for residuals, numeric explanatory variables
#linearly related to response variable, residuals norm. dist.
plot(mr$fitted.values, mr$residuals, xlab = "Fitted Values", 
     ylab = "Residuals", main = "Residual Plot", pch = 20)
abline(h = 0, col = "red")

# View results of GLM AKA Multiple Regression (MR)
summary(mr)

#interaction plot for numeric/categorical

library(car)
#install gplots
install.packages("gplots")
library(gplots)


male.yes <- stats[stats$Sex == 'Male', ]
male.no <- stats[stats$Sex == 'Female', ]

plot(male.yes$Cups.of.milk.drank.weekly, male.yes$Height..inches.,
     pch = 17, col = 'red', cex = .8, main = 'Height by Milk Drank',
     xlab = 'Milk Drank (cups)', ylab =  'Height (in)',
     ylim = c(45,80))
points(male.no$Cups.of.milk.drank.weekly, male.no$Height..inches.,
       col = 'blue', cex = .8)
abline(lm(Height..inches. ~ Cups.of.milk.drank.weekly, data = male.yes),
       col = 'red')
abline(lm(Height..inches. ~ Cups.of.milk.drank.weekly, data = male.no),
       col = 'blue', lty = 2)
legend("bottomright", title="Male?", c("Yes", "No"),
       col=c("red", "blue"), pch=c(17, 16), inset=0.01)
