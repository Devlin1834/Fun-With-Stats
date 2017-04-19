## This file deals with the HAPPY x TRAINED dataset
## I asked employees of west carts whether they enjoy working here and if
## they felt they were trained effectively. The analysis is below

setwd("D:/Files/Programing and Data/R Directory/Work Surveys")

library(ggplot2)

# First we set up our Contingency Table
HT <- read.csv("HappyTrained.csv")
HT$Happy <- factor(x = HT$Happy, levels = c("Yes", "No"))
HT$Trained <- factor(x = HT$Trained, levels = c("Yes", "No"))
HT.table <- xtabs(formula = Count ~ Happy + Trained, data = HT) 
ht.sample.sum <- sum(HT.table)

## Now I want to be able to see the column sums
## They wont be added to the table, but are still useful to call upon
names.t <- c('Trained', 'Not Trained')
names.h <- c('Happy', 'Not Happy')
ht.sum <- sum(HT.table[,1])
nht.sum <- sum(HT.table[,2])
ht.sums <- c(ht.sum, nht.sum)
ht.props <- ht.sums / ht.sample.sum
HT.col.sums <- data.frame(names.h, ht.sums, ht.props)

## Here are the Row Sums
## This gives me info about Trained
sum.ht <- sum(HT.table[1,])
sum.nht <- sum(HT.table[2,])
sums.ht <- c(sum.ht, sum.nht)
props.ht <- sums.ht / ht.sample.sum
HT.row.sums <- data.frame(names.t, sums.ht, props.ht)

## Now I want to view the proportions on my full table
HT.hat <- round(HT.table / ht.sample.sum, 2)

## Now I want a table that details the proprtion of each cell divided by its row sum
HT.r.hat <- round(HT.table / rowSums(HT.table), 2)

#################################################################
##             Lets Review What I Have So Far                  ##

## The Table
HT.table

## The Row Summary
## Details Sample Respondants Happiness
HT.row.sums

## The Column Summary
## Details Sample Respondants Training
HT.col.sums

## The table of proportions
HT.hat

## The table of intra-Row proportions
HT.r.hat

##                                                             ##
#################################################################

## OBSERVATIONS
## Happiness and Training seem to be related....
## Still lots of people who were well trained and are still unhappy
## Suggests more variables influence happiness than just training

## GOALS:
### Odds Ratio
### Chi Sq Test
### Graph
### Fit a Logit Model

## Starting with whats easy
chisq.test(HT.table, correct = FALSE)
## Easily reject the null hypothesis of independance

## Odds Ratio
HT.or <- round((HT.table[1,1] * HT.table[2,2]) / (HT.table[2,1] * HT.table[1,2]), 2)
HT.or
## This implies that the odds of an emplyee Enjoying work at west carts
## are 16.65 times higher if the employee has been properly trained
## WOWZA

## Now to fit a Logit Model
HT.model <- glm(Happy ~ Trained, binomial(link = 'logit'), data = HT.table)
summary(HT.model)
## this model is the worst model I have ever seen in my entire life


ggplot(HT, aes(x = Trained, y = Count, fill = Happy)) + 
	geom_bar(position = "fill", stat = "identity") + 
	labs(y = 'Proportion', title = 'Job Enjoyment and Effectiveness of Training')

## I want to take another stab at this
HT.binom <- HT
HT.binom$Happy <- ifelse(HT.binom$Happy == "Yes", 1, 0)
HT.binom$Trained <- ifelse(HT.binom$Trained == "Yes", 1, 0)
HT.binom$Count <- HT.binom$Count / sum(HT.binom$Count)
HT.b.mod <- glm(Count ~ Happy, binomial(link = 'logit'), data = HT.binom)
summary(HT.b.mod)
## This still is not working
## Lets add weights
HT.fit <- glm(Happy ~ Trained, weights = Count, binomial(link = logit), data = HTb)
summary(HT.fit)
## Alright, Its working!
## Intercept is not signifigant

exp(HT.fit$coefficients[2])

## Lets plot!
curve(expr = exp(HT.fit$coefficients[1] + HT.fit$coefficients[2] * x) / 
		(1 + exp(HT.fit$coefficients[1] + HT.fit$coefficients[2] * x)),
	col = 'darkgreen', main = expression(logit(pi) == -0.8109 + 2.8124*x),
	ylab = expression(pi), xlab = expression(x[1]),
	ylim = c(0, 1))

vcov(HT.fit)

beta.ci <- confint.default(HT.fit, HT.fit$coefficients[2], 0.95)
exp(beta.ci)

## Now for interpretation

pred.t0 <- HT.fit$coefficients[1] + HT.fit$coefficients[2] * 0
pred.t1 <- HT.fit$coefficients[1] + HT.fit$coefficients[2] * 1

as.numeric(exp(pred.t0)/(1 + exp(pred.t0)))
as.numeric(exp(pred.t1)/(1 + exp(pred.t1)))

# So for a person who is poorly trained, they have a probability of being
## happy of 30%
# And for a well trained person, they have a probality of being happy
## of 88%