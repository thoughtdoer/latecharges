### Grab some packages. I liked xtable for bringing summary statistics into LaTeX which was required for the class.
### You can probably skip xtable and use knitr instead.  
install.packages("xtable")
install.packages("tidyverse")
library(xtable)
library(tidyverse)

### Loan the Data ### 
### The data was obtained from a credit union in NYS. It contained approx. 4,700 recoreds. 
lateData <- read.csv("LateChargeStats.csv")

### Making multiple sub tables is redundent. As an error prevention method it was easier for me to be explicit 
### with the table naming so I was absolutely certain which variables I was working with.

## All Mortgages 
feeTable <- table(lateData$FeeCount)
barplot(feeTable,main="Late Fees - All Mortgages", xlab = "# of Fees", ylab = "# of Mortgages", ylim=c(0,4000))

# All Mortgages with 1 Fee or more 
feeTableOverZero <- table(lateData$FeeCount[!lateData$FeeCount==0])
barplot(feeTableOverZero,main="Late Fees - All Mortgages with 1 Fee or More", xlab = "# of Fees", ylab = "# of Mortgages",ylim=c(0,375))

# All Mortgage Terms 
termTable <- table(lateData$Term)
barplot(termTable,main="Breakdown of Loan Terms", xlab = "Term of Loan", ylab = "# of Mortgages", ylim=c(0,2500))

# All Int Rates
rateTable <- table(lateData$IntRate)
barplot(rateTable,main="Breakdown of Interest Rates", xlab = "Interest Rate of Loan", ylab = "# of Mortgages", ylim=c(0,500))

# Age 
hist(lateData$Age,main="Age of Primary Borrowers", xlab = "Age in Years", ylab = "# of Primary Borrowers", ylim=c(0,800), xlim=c(20,100), col="grey")
sd(lateData$Age)
mean(lateData$Age)

# Credit Score
hist(lateData$CreditScore,main="Credit Score of Primary Borrowers", xlab = "Credit Score", ylab = "# of Primary Borrowers", ylim=c(0,1300), xlim=c(600,875), col="grey")
creditTable <- table(lateData$CreditScore)
creditTable 
sd(lateData$CreditScore)
mean(lateData$CreditScore)

# Gender 
genderTable <- table(lateData$Gender)
xtable(genderTable)

### At one point I considered using a pie chart to display this. Then I met Tufte. 
#genderLabels <- c("Male","Female")
#pie(genderTable, main="Gender: Male or Female", labels = genderLabels)

# Race 
raceTable <- table(lateData$IsWhite)
xtable(raceTable)
### At one point I considered using a pie chart to display this. Then I met Tufte. 
#raceLabels <- c("White","Non-White")
#pie(raceTable, main="Race: White or Non White", labels = raceLabels)


# Linear Regression 
lateReg <- lm(FeeCount ~ IntRate + Term + CreditScore + Age + Gender + IsWhite, data=lateData)
summary(lateReg)
par(mfrow=c(2,2))
plot(lateReg)
xtable(lateReg)

### It's usually good form to cite the authors of packages used. 
### Use the citation function to create a BibTex entry. Do it once and reuse it in the future. 
citation()
citation(package = "xtable")
citation(package = "tidyverse")
