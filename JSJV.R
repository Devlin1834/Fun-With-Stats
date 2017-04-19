setwd("D:/Files/Programing and Data/R Directory/Work Surveys")

library(plotly)

JS <- read.csv("JobShift.csv")
JV <- read.csv("JobVenue.csv")

JS$Job <- factor(x = JS$Job, levels = c("Cashier", "Cook", "Stocker", "Captain"))
JS$Shift <- factor(x = JS$Shift, levels = c("Open", "Mid", "Close"))
JS.table <- xtabs(formula = Count ~ Job + Shift, data = JS)
table.sum <- sum(JS.table)
JS.r.hat <- JS.table / rowSums(JS.table)
JS.t.hat <- round(JS.table / table.sum, 2)
summary(JS.table)


open.sum <- sum(JS.table[,1])
mid.sum <- sum(JS.table[,2])
close.sum <- sum(JS.table[,3])
column.sums <- c(open.sum, mid.sum, close.sum)
JS.c.hat <- t(t(JS.table) / column.sums)

JS.col.props <- colSums(JS.table) / table.sum
open.prop <- as.vector(JS.col.props[1])
else.prop <- as.vector(sum(JS.col.props[2:3]))

SE <- sqrt(((open.prop*(1-open.prop))/open.sum)+((else.prop*(1-else.prop))/(close.sum+mid.sum)))
z <- (open.prop - else.prop) / SE
pnorm(z)

prop.test(x = c(open.sum, (mid.sum+close.sum)), n = c(table.sum, table.sum), correct = FALSE)

################################################
############### Tables generated ###############
################################################
                                               #
### The Full Table                             #
JS.table                                       #
### Counts as a proportion of Row Sums         #
JS.r.hat                                       #
### Counts as a proportion of Column Sums      #
JS.c.hat                                       #
### Counts as a proportion of Table Sums       #
JS.t.hat                                       #
                                               #
################################################

barplot(JS.r.hat[,1],
	  main = "Open Prefference",
	  xlab = "Frequency",
	  ylab = "Job",
	  horiz = "TRUE",
	  col = c("green4", "grey55", "wheat", "blue4"),
	  xaxp  = c(0.0, .85, 17)
)

Shift <- colnames(JS.table)
Cashier <- as.vector(JS.table[1,])
Cook <- as.vector(JS.table[2,])
Stocker <- as.vector(JS.table[3,])
Captain <- as.vector(JS.table[4,])
JS.data <- data.frame(Shift, Cashier, Cook, Stocker, Captain)
JS.plot <- plot_ly(JS.data,
			 x = ~Shift, 
			 y = ~ Cashier,
			 type = 'bar',
			 name = 'Cashier') %>%
	add_trace(y = ~ Cook, name = 'Cook') %>%
	add_trace(y = ~ Stocker, name = 'Stocker') %>%
	add_trace(y = ~ Captain, name = 'Captain') %>%
	layout(yaxis = list(title = 'Count'), barmode = 'group')
JS.plot

###########################################################################################

JV$Job <- factor(x = JV$Job, levels = c("Cashier", "Cook", "Stocker", "Captain"))
JV$Venue <- factor(x = JV$Venue, levels = c("Pred", "Grill", "Fruit", "Kong", "Turkey", "Beer"))
JV.table <- xtabs(formula = Count ~ Job + Venue, data = JV)
JV.r.hat <- JV.table / rowSums(JV.table)
table.sum <- sum(JV.table)
JV.t.table <- round(JV.table / table.sum, 2)

################################################
############### Tables generated ###############
################################################
                                               #
### The Full Table                             #
JV.table                                       #
### Counts as a proportion of Row Sums         #
JV.r.hat                                       #                                       
### Counts as a proportion of Table Sums       #
JV.t.hat                                       #
                                               #
################################################

x11()
barplot(JV.r.hat[1,],
	  main = "Cashier Venue Prefference",
	  xlab = "Frequency",
	  ylab = "Venue",
	  horiz = "TRUE",
	  col = c("green4", "orange2", "white", "grey55", "red3", "blue4")
)

### Lets partition the chi-sq

R.Cashiers <- sum(JV.table[1,1:2])
C.Cashiers <- sum(JV.table[1,3:6])
R.Else <- sum(JV.table[2:4,1:2])
C.Else <- sum(JV.table[2:4,3:6])

p.table <- array(
	data = c(R.Cashiers, C.Cashiers, R.Else, C.Else),
	dim = c(2,2),
	dimnames = list(
		Job = c("Cashier", "Else"),
		Venue = c("Restaraunt", "Cart")))
summary(p.table)
prop.test(x = p.table, conf.level = .95, correct = FALSE)
