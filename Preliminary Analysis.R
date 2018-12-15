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

hist(stats$Cups.of.milk.drank.weekly, xlim=c(0,25), breaks=11, xlab =
       'cups of milk', main = 'Cups of milk drank by UT class 2020')
summary(stats$Cups.of.milk.drank.weekly)
