## This file deals with the JOB x HAPPY dataset
## I asked employees of west carts what their current job was and if
## they were happy working here. The analysis is below

setwd("D:/Files/Programing and Data/R Directory/Work Surveys")

library(ggplot2)

# First we set up our Contingency Table
JH <- read.csv("JobHappy.csv")
JH$Job <- factor(x = JH$Job, levels = c("Cashier", "Stocker", "Busser", "Line.Cook"))
JH$Happy <- factor(x = JH$Happy, levels = c("Yes", "No"))
JH.table <- xtabs(formula = Count ~ Job + Happy, data = JH) 
h.sample.sum <- sum(JH.table)

## Now I want to be able to see the column sums
## They wont be added to the table, but are still useful to call upon
happy.sum <- sum(JH.table[,1])
no.sum <- sum(JH.table[,2])
happynot.sums <- c(happy.sum, no.sum)
names.h <- c("Happy", "Not")
happy.props <- happynot.sums / h.sample.sum
JH.col.sums <- data.frame(names.h, happynot.sums, happy.props)

## Here are the Row Sums
## This just gives me info about my sample
cash.sum.h <- sum(JH.table[1,])
stock.sum.h <- sum(JH.table[2,])
buss.sum.h <- sum(JH.table[3,])
cook.sum.h <- sum(JH.table[4,])
h.job.sums <- c(cash.sum.h, stock.sum.h, buss.sum.h, cook.sum.h)
h.job.props <- h.job.sums / h.sample.sum
JH.row.sums <- data.frame(names, h.job.sums, h.job.props)

## Now I want to view the proportions on my full table
JH.hat <- round(JH.table / h.sample.sum, 2)

## Now I want a table that details the proprtion of each cell divided by its row sum
JH.r.hat <- round(JH.table / rowSums(JH.table), 2)

#################################################################
##             Lets Review What I Have So Far                  ##

## The Table
JH.table

## The Row Summary
## Details Sample Respondants Current Job
JH.row.sums

## The Column Summary
## Details Sample Respondants Cross Train Prefferences
## Ignores influence of current job - NOT GOOD STATS
JH.col.sums

## The table of proportions
JH.hat

## The table of intra-Row proportions
JH.r.hat

##                                                             ##
#################################################################

## Lets make some Observations
## Bussers Not Happy
## Stockers very happy
## overall, venue 75% happy

## GOALS:
### Test of Independance
### Pretty Graph

## Starting iwth a test of Independance
chisq.test(JH.table, correct = FALSE)
## Fail to reject the Null with pearsons chi-squared
fisher.test(JH.table)
## Same Result

## Graph
ggplot(JH, aes(x = Job, y = Count, fill = Happy)) + 
	geom_bar(position = "dodge", stat = "identity") + 
	labs(y = 'Count', title = 'Do You Enjoy Working Here?')




