cutlets <- read.csv(file.choose())
View(cutlets)
colnames(cutlets) <- c("UnitA","UnitB")
View(cutlets)
attach(cutlets)
### Y is Continuous & X is Discrete ####
## Compare 2 Population with each other 
#####Normality test######
shapiro.test(UnitA)
# p-value = 0.32 >0.05 so p high null fly => It follows normal distribution
shapiro.test(UnitB)
# p-value = 0.5225 >0.05 so p high null fly => It follows normal distribution
## No External Conditions so go for Paired T test
### Paired T test
cutlet <- t.test(UnitA,UnitB,paired=TRUE,conf.level = 0.95)
cutlet
# pvalue=0.4562 > 0.05 so we can reject alternate hypothesis(H1) 
# Select Null Hypothesis and conclude that there isn't any significant 
# difference in the diameter of the cutlet between two units. 



#Laboratory Test(LabTAT)
labtat <- read.csv(file.choose())
View(labtat)
colnames(labtat) <- c("lab1","lab2","lab3","lab4")
View(labtat)
attach(labtat)
## Y is discerte and X is continuous ##
## More than 2 populations
### Normality Test ###
shapiro.test(lab1)
# p value=0.5508 > 0.05 so p high null fly => It follows normal distribution
shapiro.test(lab2)
# p value=0.0.8637 > 0.05 so p high null fly => It follows normal distribution
shapiro.test(lab3)
# p value=0.4205 > 0.05 so p high null fly => It follows normal distribution
shapiro.test(lab4)
# p value=0.6619 > 0.05 so p high null fly => It follows normal distribution

# Time is an External condition so go for variance test

######Variance test######
### Bartlett Test ###
Stack_Data <- stack(labtat)
View(Stack_Data)
attach(Stack_Data)
bartlett.test(values~ind, data=Stack_Data)
# Equal Variance
# pvalue =0.1069 > 0.05 p high null fly accept Null Hypothesis

##### Mean Test #####
###ANOVA Test ####
Anova_lab <- aov(values~ind,data=Stack_Data)
summary(Anova_lab)
## p value=2e-16 < 0.05 accept Alternate Hypothesis
# Null Hypothesis=All means are Equal 
# Alternate Hypothesis= All means are not equal 
# Conclude that there is difference in the average Turn Around Time (TAT)
# of reports of the laboratories on Hospital's preferred list. 


##Buyer Ratio 
Buy_rat <- read.csv(file.choose())
View(Buy_rat)
attach(Buy_rat)

# Input Variable,X is discrete (EAst,West,North,South)
# Output Variable,Y is also discrete ( Male or Female are simliar or not across
# regions)
# Y is discrete and X is discrete
### More than 2 Populations ###
##### Chi-Square Test #####
stack_buy <- stack(Buy_rat)
View(stack_buy)
attach(stack_buy)

chisq.test(values,ind)
## p value=0.2931 > 0.05 Accept Null Hypothesis
## Conclude that propotions of Male and Female are similar across regions.


####### Customer Order Form #######
Cust <- read.csv(file.choose())
View(Cust)
colnames(Cust) <- c("Phil","Indo","Mal","Ind")
attach(Cust)
Cust$Phil <- ifelse(Cust$Phil=="Error Free" ,1,0)
Cust$Indo <- ifelse(Cust$Indo=="Error Free" ,1,0)
Cust$Mal <- ifelse(Cust$Mal=="Error Free" ,1,0)
Cust$Ind <- ifelse(Cust$Ind=="Error Free" ,1,0)
View(Cust)
stack_Cust <- stack(Cust)
View(stack_Cust)
attach(stack_Cust)
# Y is discrete and X is discrete
### More than 2 Populations ###
##### Chi-Square Test #####
chisq.test(values,ind)
# p value = 2.2e-16 < 0.05 Accept Alternate Hypothesis
# Manager can come to Conclusion that defective % varies among 4 centres. 


### Fantaloons ####
Fant <- read.csv(file.choose())
View(Fant)
Fant$Weekdays <- ifelse(Fant$Weekdays=="Male",1,0)
Fant$Weekend <- ifelse(Fant$Weekend=="Male",1,0)
Stack_Fant <- stack(Fant)
table_Fant <- table(Stack_Fant)
# Y is discrete and X is discrete
### 2 Populations ###
##### 2 Propotion Test #####
prop.test(x=c(113,287),n=c(280,520),alternative = "two.sided",conf.level = 0.95,correct = FALSE)
# two. sided -> means checking for equal proportions of Male and Female under Weekday.
# p value = 6.261e-16 < 0.05 Accept Alternate Hypothesis i.e
# Unqual Proportions
prop.test(x=c(113,287),n=c(280,520),alternative = "less",conf.level = 0.95,correct = FALSE)
# p value= 3.131e-05 Accept Alternate Hypothesis
# Ho -> Proportions of Female >equal Proportions of Male
# Ha -> Proportions of MAle > Proportions of Female
# so Proportion of Female < Proportion of Male 
