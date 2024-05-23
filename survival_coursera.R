install.packages("survival")
install.packages("survminer")

library(survival)
library(survminer)

# Load necessary library
library(dplyr)
data <- read.csv(file = "survival_data.csv",
              header=TRUE, sep=',')

# Check the lengths of all variables
length(data$fu_time)
length(data$death)
length(data$age)
length(data$gender)
length(data$copd)
length(data$prior_dnas)
length(data$ethnicgroup)

# Check for missing values
sum(is.na(data$fu_time))
sum(is.na(data$death))
sum(is.na(data$age))
sum(is.na(data$gender))
sum(is.na(data$copd))
sum(is.na(data$prior_dnas))
sum(is.na(data$ethnicgroup))

# Print the number of rows in the dataframe
nrow(data)


cox <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = data) # take variables straight from g
summary(cox)

ethnicgroup <- factor(data[,"ethnicgroup"]) # can also use “as.factor” rather than “factor”
levels(ethnicgroup)<-c(levels(ethnicgroup),"8") # add level 8 to the factor

ethnicgroup[is.na(ethnicgroup)] <- "8" # Change NA to "None"
fu_time <- data[,"fu_time"]
death <- data[,"death"]
copd <- data[,"copd"]
prior_dnas <- data[,"prior_dnas"]
age <- data[,"age"]
gender <- data[, "gender"]

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

t <- table(gender, exclude=NULL)
addmargins(t) # adds the total (a "sum" column)

round(100*prop.table(t),digits=1)

t <- table(copd, exclude=NULL)

addmargins(t) # adds the total (a "sum" column)



cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + prior_dnas + ethnicgroup)

summary(cox)

quintile <- data[,"quintile"]

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup) 

summary(cox)

table(quintile, exclude=NULL) 

quintile_5groups <- data[,"quintile"] # best start again with the original data set, not from the existing object called “quintile” 

quintile_5groups[quintile_5groups==0] <- 5 # This picks the individuals with quintile=0 (note the double equals sign) and sets them to 5

quintile_5groups <- factor(quintile_5groups) # lastly, tell R that this is a categorical variable and not a continuous one

quintile <- relevel(quintile, ref = 2) # quintile 1 as the ref cat again

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup)

summary(cox)


# now run the model with this new variable 

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile_5groups + ethnicgroup) 

summary(cox) 


fit <- coxph(Surv(fu_time, death) ~ gender) # fit the desired model

temp <- cox.zph(fit)# apply the cox.zph function to the desired model

print(temp) # display the results

plot(temp) # plot the curves
