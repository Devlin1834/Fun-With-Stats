## This file deals with the JOB x TRAINED dataset
## I asked employees of west carts what their current job was and if
## they felt they were trained effectively. The analysis is below

setwd("D:/Files/Programing and Data/R Directory/Work Surveys")

library(ggplot2)

# First we set up our Contingency Table
JT <- read.csv("JobTrain.csv")
JT$Job <- factor(x = JT$Job, levels = c("Cashier", "Stocker", "Busser", "Line.Cook"))
JT$Trained <- factor(x = JT$Trained, levels = c("Yes", "No"))
JT.table <- xtabs(formula = Count ~ Job + Trained, data = JT) 
t.sample.sum <- sum(JT.table)

## Now I want to be able to see the column sums
## They wont be added to the table, but are still useful to call upon
train.sum <- sum(JT.table[,1])
not.sum <- sum(JT.table[,2])
trainnot.sums <- c(train.sum, not.sum)
names.t <- c("Trained", "Not")
train.props <- trainnot.sums / t.sample.sum
JT.col.sums <- data.frame(names.t, trainnot.sums, train.props)

## Here are the Row Sums
## This just gives me info about my sample
cash.sum.t <- sum(JT.table[1,])
stock.sum.t <- sum(JT.table[2,])
buss.sum.t <- sum(JT.table[3,])
cook.sum.t <- sum(JT.table[4,])
t.job.sums <- c(cash.sum.t, stock.sum.t, buss.sum.t, cook.sum.t)
t.job.props <- t.job.sums / t.sample.sum
JT.row.sums <- data.frame(names, t.job.sums, t.job.props)

## Now I want to view the proportions on my full table
JT.hat <- round(JT.table / t.sample.sum, 2)

## Now I want a table that details the proprtion of each cell divided by its row sum
JT.r.hat <- round(JT.table / rowSums(JT.table), 2)

#################################################################
##             Lets Review What I Have So Far                  ##

## The Table
JT.table

## The Row Summary
## Details Sample Respondants Current Job
JT.row.sums

## The Column Summary
## Details Sample Respondants Cross Train Prefferences
## Ignores influence of current job - NOT GOOD STATS
JT.col.sums

## The table of proportions
JT.hat

## The table of intra-Row proportions
JT.r.hat

##                                                             ##
#################################################################

## Lets make some observations
## Most people feel well trained
## Except Bussers
## Cooks and Stockers recieve above average training
## Cashiers and Bussers recieve below average training
## There isnt much here thats interesting

## GOALS:
### Test of independance
### Graph

## Here is a Chi Squared Test of Independance
chisq.test(JT.table, correct = FALSE)
## We fail to reject the null here, That much is obvious from looking at the data
fisher.test(JT.table)
## Same

## Graph
ggplot(JT, aes(x = Job, y = Count, fill = Trained)) + 
	geom_bar(position = "dodge", stat = "identity") + 
	labs(y = 'Count', title = 'Do You Feel You Were Trained Effectively?')
