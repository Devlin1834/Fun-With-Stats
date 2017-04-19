## This file deals with the JOB x CROSS TRAIN dataset
## I asked employees of west carts what their current job was and what job they
## would cross tain into if they had to. The analysis is below

setwd("D:/Files/Programing and Data/R Directory/Work Surveys")

library(ggplot2)

# First we set up our Contingency Table
JX <- read.csv("JobCross.csv")
JX$Job <- factor(x = JX$Job, levels = c("cashier", "stocker", "busser", "line.cook"))
JX$Cross.Train <- factor(x = JX$Cross.Train, levels = c("cashier", "stocker", "busser", "line.cook"))
JX.table <- xtabs(formula = Count ~ Job + Cross.Train, data = JX) 
sample.sum <- sum(JX.table)


## Now I want to be able to see the column sums
## They wont be added to the table, but are still useful to call upon
cash.sum.x <- sum(JX.table[,1])
stock.sum.x <- sum(JX.table[,2])
buss.sum.x <- sum(JX.table[,3])
cook.sum.x <- sum(JX.table[,4])
xtrain.sums <- c(cash.sum.x, stock.sum.x, buss.sum.x, cook.sum.x)
names <- c("cashier", "stocker", "busser", "cook")
xtrain.props <- xtrain.sums / sample.sum
JX.col.sums <- data.frame(names, xtrain.sums, xtrain.props)

## Here are the Row Sums
## This just gives me info about my sample
cash.sum.c <- sum(JX.table[1,])
stock.sum.c <- sum(JX.table[2,])
buss.sum.c <- sum(JX.table[3,])
cook.sum.c <- sum(JX.table[4,])
job.sums <- c(cash.sum.c, stock.sum.c, buss.sum.c, cook.sum.c)
job.props <- job.sums / sample.sum
JX.row.sums <- data.frame(names, job.sums, job.props)

## Now I want to view the proportions on my full table
JX.hat <- round(JX.table / sample.sum, 2)

## Now I want a table that details the proprtion of each cell divided by its row sum
JX.r.hat <- round(JX.table / rowSums(JX.table), 2)

#################################################################
##             Lets Review What I Have So Far                  ##

## The Table
JX.table

## The Row Summary
## Details Sample Respondants Current Job
JX.row.sums

## The Column Summary
## Details Sample Respondants Cross Train Prefferences
## Ignores influence of current job - NOT GOOD STATS
JX.col.sums

## The table of proportions
JX.hat

## The table of intra-Row proportions
JX.r.hat

##                                                             ##
#################################################################

## Lets make some observations
## Cashiers want to be cooks and stockers
## Cooks want to be stockers
## Bussers want to be cooks
## Stockers want to be cooks

## GOALS:
### Rank Venue Job Prefferences
### Test Independance
### Make 1 Pretty Chart in GGPlot2

## Start where its easy: Testing Independance:
chisq.test(JX.table, correct = FALSE)
## BAM that what I like
## Easily reject the null and conclude dependance
fisher.test(JX.table)
## these results are fairly obvious
## of course cross.train is dependant upon your current job
## because you cant cross train into your current job
chisq.test(JX.table, correct = FALSE)$residual
## The residuals prove my point

## Ranking Venue Job Preference
## Fairly Straighforward
### 1. Cook
### 2. Stocker
### 3. Busser
### 4. Cashier
#### Why?
##### More cashiers than anyone else in study dilutes possible cashier responses
##### Still doesnt account for the proportions that are heavily in favor of stocking and cooking

## GGPlot Graph  
JX.p <- JX
r.hat <- as.vector(t(JX.r.hat))
r.props <- r.hat[-c(1, 6, 11, 16)]
JX.p[3] <- r.props
ggplot(JX.p, aes(x = Job, y = Count, fill = Cross.Train)) + 
	geom_bar(position = "dodge", stat = "identity") + 
	labs(y = 'Proportion', title = 'Intra-Job Cross Train Proportions')


