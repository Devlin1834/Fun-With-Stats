setwd("D:/Files/Programing and Data/R Directory/Work Surveys")

library(ggplot2)

############################################################
#                                                          #  
#  ##### #   # #   #         #       # ##### ##### #   #   #
#  #     #   # ##  #         #       #   #     #   #   #   #
#  ####  #   # # # #         #   #   #   #     #   #####   #
#  #     #   # #  ##         #  # #  #   #     #   #   #   #
#  #      ###  #   #          ##   ##  #####   #   #   #   #
#                                                          #
#  ###  #########     #      #########  ###    #########   #
# #   #     #        # #         #     #   #    #  #  #    #
# #         #       #   #        #     #        #  #  #    #
#  ###      #      #     #       #      ###     #  #  #    #
#     #     #     #########      #         #    #  #  #    #
# #   #     #    #         #     #     #   #    #  #  #    #
#  ###      #   #           #    #      ###    #########   #
#                                                          #
############################################################

fws <- read.csv('fws3.csv')
head(fws)
summary(fws)

## Survey Asked 5 Questions
### 1. What is Your Job?
#### JOB
### 2. How long have you worked at univesal
#### VETERENCY
### 3. Do you feel valued at work
#### VALUE
### 4. Do you feel expectations for you are clearly communicated
#### EXPECTATIONS
### 5. Should cheesey garlic pizza be banned from the minigrill
#### GARLIC

## From this I want to examine 8 relationships
### 1. Veterency ~ Job
### 2. Expectations ~ Job
### 3. Value ~ Job
### 4. Garlic ~ Job
### 5. Value ~ Veterency
### 6. Expectations ~ Veterency
### 7. Garlic ~ Veterency
### 8. Value ~ Expectations

## To rephrase in terms of questions
### Q1
#### 4 as Predictor
#### 0 as Response
### Q2
#### 3 as Predictor
#### 1 as Response
### Q3
#### 2 as Predictor
#### 1 as Response
### Q4
#### 1 as Predictor
#### 2 as Response
### Q5
#### 0 as Predictor
#### 2 as Response

#################################
## PART A: Global Deffinitions ##
#################################
j.names <- c('Cashier', 'Busser', 'Stocker', 'Line Cook')

westcarts_theme <- theme(
			 plot.title = element_text(face = 'bold.italic', size = '18', color = 'gray17'),
			 axis.title = element_text(face = 'bold', size = '12', color = 'black'),
			 axis.text = element_text(face = 'italic', size = '10', color = 'black'),
			 axis.ticks = element_blank(),
			 panel.background = element_rect(fill = 'white', color = 'white'),
			 panel.grid.major.x = element_blank(),
			 panel.grid.major.y = element_line(linetype = 2, color = 'darkgreen'),
			 panel.grid.minor.x = element_blank(),
			 panel.grid.minor.y = element_blank()
			 )

#############################
## PART 1: Veterency ~ Job ##
#############################
# First we examine the means
c.data <- fws[which(fws$Job == 0),]                 # Filter out Cashiers
c.vet.clean <- c.data[complete.cases(c.data[,2]),]  # Ditch the NA's
c.vet.mean <- mean(c.vet.clean$Veterency)           # Calculate a Mean
                                                    # Repeat
b.data <- fws[which(fws$Job == 1),]
b.vet.clean <- b.data[complete.cases(b.data[,2]),]
b.vet.mean <- mean(b.vet.clean$Veterency)

s.data <- fws[which(fws$Job == 2),]
s.vet.clean <- s.data[complete.cases(s.data[,2]),]
s.vet.mean <- mean(s.vet.clean$Veterency)

l.data <- fws[which(fws$Job == 3),]
l.vet.clean <- l.data[complete.cases(l.data[,2]),]
l.vet.mean <- mean(l.vet.clean$Veterency)

j.vet.means <- c(c.vet.mean, b.vet.mean, s.vet.mean, l.vet.mean)
means.comp <- data.frame(j.names, j.vet.means)
#--------#
means.comp
#--------#

# Now lets model
jobsvet.clean <- fws[complete.cases(fws[,1:2]),]
jobs.vet <- data.frame(C = c(1:49),
			 B = c(1:49),
			 S = c(1:49),
			 L = c(1:49))
jobs.vet[1] <- ifelse(jobsvet.clean[1] == 0, 1, 0)
jobs.vet[2] <- ifelse(jobsvet.clean[1] == 1, 1, 0)
jobs.vet[3] <- ifelse(jobsvet.clean[1] == 2, 1, 0)
jobs.vet[4] <- ifelse(jobsvet.clean[1] == 3, 1, 0)
jobs.vet[5] <- jobsvet.clean[2]
colnames(jobs.vet) <- c('JobCashier', 'JobBusser', 'JobStocker', 'JobCook', 'Vet')
jobvet.fit <- lm(Vet ~ JobCashier + JobBusser + JobStocker + JobCook, data = jobs.vet)
summary(jobvet.fit)
# this model is terrible
# Lets make a box and whisker plot
jobs.graph <- jobsvet.clean[,1:2]
jobs.graph[1] <- as.factor(jobs.graph$Job)

ggplot(jobs.graph, aes(x = Job, y = Veterency, fill = Job)) + 
	scale_x_discrete(labels = c('Cashier', 'Busser', 'Stocker', 'Cook')) +
	scale_fill_manual(values = c('powderblue', 'lightyellow', 'palegreen', 'pink'),
				name = 'Job', labels = c('Cashier', 'Busser', 'Stocker', 'Cook')) +
	geom_hline(aes(yintercept=1), linetype = 2, color = 'red') + 
	guides(fill = FALSE) + 
	geom_boxplot() + labs(title = 'Distribution of Veterency Based upon Job') + westcarts_theme

ggplot(jobs.graph, aes(x = Veterency, fill = Job)) + geom_histogram(bins = 19) + 
	scale_fill_manual(values = c('powderblue', 'lightyellow', 'palegreen', 'pink'),
				name = 'Job', labels = c('Cashier', 'Busser', 'Stocker', 'Cook')) +
	scale_x_continuous(breaks = c(0:18)) + 
	labs(title = 'Distribution of Veterency at West Carts', 
	     subtitle = 'How long have you worked for Universal?') + 
	westcarts_theme + theme(panel.grid.major.x = element_line(linetype = 2, color = 'grey75'))
	
################################
## PART 2: Expectations ~ Job ##
################################
# First we construct a table
j.ex.clean <- fws[complete.cases(fws[,c(1,4)]),]                   # Filter out NA's
j.ex.table <- xtabs(~Job + Expectations, data = j.ex.clean)        # Create a Table
colnames(j.ex.table) <- c('No', 'Yes')                             # Rename Columns
rownames(j.ex.table) <- c('Cashier', 'Busser', 'Stocker', 'Cook')  # Rename Rows
#--------#
j.ex.table
#--------#
chisq.test(j.ex.table, correct = FALSE)                            # Dependant                       
fisher.test(j.ex.table)                                            # Dependant                       
# Cashiers are the only ones that vary
j.ex.ptable <- j.ex.table / rowSums(j.ex.table)
#---------#
j.ex.ptable
#---------#

curve(dbinom(0,2,x), xlim = c(0,1), ylim = c(0,1),
	main = 'Likelihood Function For Stockers', 
	sub = 'Do you feel expectations for you are clearly communicated?',
	xlab = expression(pi), ylab = 'Likelihood', col = 'navy')
abline(v = 0.25, lty = 'dashed', col = 'firebrick')
abline(v = 0, lty = 'dashed', col = 'gold')

ex.graph <- j.ex.clean[,c(1,4)]
ex.graph[1] <- as.factor(ex.graph$Job)
ex.graph[2] <- as.factor(ex.graph$Expectations)
ex.graph[2] <- ifelse(ex.graph[2] == 1, 'Yes', 'No')

ggplot(ex.graph, aes(x = Job, fill = Expectations)) +  geom_bar() + 
	labs(title = 'Clarity of Expectation Communication',
	     subtitle = 'Do you feel expectations for you are clearly communicated?') + 
	scale_fill_manual(values = c('firebrick', 'navy'))+ 
	scale_x_discrete(labels = c('Cashier', 'Busser', 'Stocker', 'Cook')) +
	westcarts_theme

#########################
## PART 3: Value ~ Job ##
#########################
j.value.clean <- fws[complete.cases(fws[,c(1,3)]),]                   # Filter out NA's
j.value.table <- xtabs(~Job + Value, data = j.value.clean)            # Construct a table
colnames(j.value.table) <- c('No', 'Yes')                             # Rename Columns
rownames(j.value.table) <- c('Cashier', 'Busser', 'Stocker', 'Cook')  # Rename Rows
#-----------#
j.value.table
#-----------#
chisq.test(j.value.table, correct = FALSE)                            # Dependant                       
fisher.test(j.value.table)                                            # Dependant                       
# Stockers and Cashierss both Vary
# stockers is easy math 
# 2/3rds feel not valued
j.value.ptable <- j.value.table / rowSums(j.value.table)
#------------#
j.value.ptable
#------------#

curve(dbinom(1,3,x), xlim = c(0,1), ylim = c(0,1),
	main = 'Likelihood Function For Stockers', 
	sub = 'Do you feel valued at work?',
	xlab = expression(pi), ylab = 'Likelihood', col = 'navy')
abline(v = 0.33, lty = 'dashed', col = 'firebrick')



value.graph <- j.value.clean[,c(1,3)]
value.graph[1] <- as.factor(value.graph$Job)
value.graph[2] <- as.factor(value.graph$Value)
value.graph[2] <- ifelse(value.graph[2] == 1, 'Yes', 'No')

ggplot(value.graph, aes(x = Job, fill = Value)) +  geom_bar() + 
	labs(title = 'Team Member Value',
	     subtitle = 'Do you feel valued at work?') + 
	scale_fill_manual(values = c('firebrick', 'navy'))+ 
	scale_x_discrete(labels = c('Cashier', 'Busser', 'Stocker', 'Cook')) +
	westcarts_theme

##########################
## PART 4: Garlic ~ Job ##
##########################
j.gar.clean <- fws[complete.cases(fws[,c(1,5)]),]
j.gar.table <- xtabs(~Job + Garlic, data = j.gar.clean)
colnames(j.gar.table) <- c('No', 'Yes')                             
rownames(j.gar.table) <- c('Cashier', 'Busser', 'Stocker', 'Cook')
#---------#
j.gar.table
#---------#  
j.gar.ptable <- j.gar.table / rowSums(j.gar.table)
#----------#
j.gar.ptable
#----------#

chisq.test(j.gar.table, correct = FALSE)                       
fisher.test(j.gar.table)

# Independance means I can condense into:
garlic.props <- colSums(j.gar.table) / sum(j.gar.table)
#----------#
garlic.props
#----------#

gar.graph <- j.gar.clean[,c(1,5)]
gar.graph[1] <- as.factor(gar.graph$Job)
gar.graph[2] <- as.factor(gar.graph$Garlic)
gar.graph[2] <- ifelse(gar.graph[2] == 1, 'Yes', 'No')

ggplot(gar.graph, aes(x = Garlic)) +  geom_bar(fill = c('firebrick', 'navy')) + 
	labs(title = 'Garlic Pizza Approval Rating',
	     subtitle = 'Should Cheesey Garlic Pizza be banned from the mini-grill?') + 
	westcarts_theme

###############################
## PART 5: Veterency ~ Value ##
###############################
vv.clean <- fws[complete.cases(fws[,c(2,3)]),]
vv.fit <- glm(Value ~ Veterency, family = binomial(link = logit), data = vv.clean)
summary(vv.fit)
# Veterency does not have a substantial effect on feeling valued

######################################
## PART 6: Veterency ~ Expectations ##
######################################
ve.clean <- fws[complete.cases(fws[,c(2,4)]),]
ve.fit <- glm(Expectations ~ Veterency, family = binomial(link = logit), data = ve.clean)
summary(ve.fit)
# Veterency does not have a substantial effect on Expectations

################################
## PART 7: Veterency ~ Garlic ##
################################
vg.clean <- fws[complete.cases(fws[,c(2,5)]),]
vg.fit <- glm(Garlic ~ Veterency, family = binomial(link = logit), data = vg.clean)
summary(vg.fit)
# Veterency does not have a substantial effect on love of garlic pizza

###################################
## PART 8: Valued ~ Expectations ##
###################################
ev.clean <- fws[complete.cases(fws[,c(3,4)]),]
ev.table <- xtabs(~Value + Expectations, data = ev.clean)
rownames(ev.table) <- c('No', 'Yes')
colnames(ev.table) <- c('No', 'Yes')
#------#
ev.table
#------#
chisq.test(ev.table, correct = FALSE)
fisher.test(ev.table)

ev.fit <- glm(Value ~ Expectations, family = binomial(link = logit), data = ev.clean)
summary(ev.fit)
curve(expr = exp(ev.fit$coefficients[1] + ev.fit$coefficients[2] * x) / 
		 (1 + exp(ev.fit$coefficients[1] + ev.fit$coefficients[2] * x)), 
		 col = 'firebrick', main = expression(logit(pi) == -0.9808 + 3.8986*x),
		 ylab = expression(pi), xlab = expression(x[1]),
		 ylim = c(0,1))
exp(ev.fit$coefficients[2])

ev.pred0 <- ev.fit$coefficients[1] + (ev.fit$coefficients[2] * 0)             # lower logit link
ev.pred1 <- ev.fit$coefficients[1] + (ev.fit$coefficients[2] * 1)             # upper logit link
as.numeric(exp(ev.pred0)/(1 + exp(ev.pred0)))                                 # 0.27
as.numeric(exp(ev.pred1)/(1 + exp(ev.pred1)))                                 # 0.94

ev.graph <- ev.clean[,c(3,4)]
ev.graph[1] <- as.factor(ev.graph$Value)
ev.graph[2] <- as.factor(ev.graph$Expectations)
ev.graph <- ifelse(ev.graph == 1, 'Yes', 'No')
ev.graph <- as.data.frame(ev.graph)

ggplot(ev.graph, aes(x = Value, fill = Expectations)) + geom_bar() + 
	labs(title = 'Expectation Communication and Feelings of Value') + 
	scale_fill_manual(values = c('firebrick', 'navy'))+
	westcarts_theme
