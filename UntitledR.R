# Load necessary library
library(dplyr)
g <- read.csv(file = "data-for-R.csv",
              header=TRUE, sep=',')
dim(g)
View(g)
chol <- g["chol"] # cholesterol is continuous, so it’s easy
gender <- as.factor(g[,"gender"]) # but gender isn’t.
dm <- as.factor(g[,"dm"]) # neither is dm


t <- table(gender) # store the tabulation for further manipulation
addmargins(t) # this will sum up the gender totals to give an overall total and print the results
round(100*prop.table(t),digits=1) # get %s rounded to 1dp


filtered_data <- g %>%
  filter(!is.na(dm) & location == "Buckingham")

# Calculate the total number of people from Buckingham with recorded diabetes status
total_buckingham <- nrow(filtered_data)

# Calculate the number of people from Buckingham with diabetes
buckingham_with_diabetes <- filtered_data %>%
  filter(dm == "yes") %>%
  nrow()

# Calculate the percentage
percentage_with_diabetes <- (buckingham_with_diabetes / total_buckingham) * 100

# Print the result
percentage_with_diabetes


# Assuming 'data' is your dataset and the relevant columns are 'Location' and 'DiabetesStatus'
# Filter data to include only those with recorded diabetes status
filtered_data <- g %>%
  filter(!is.na(dm))

# Recode 'DiabetesStatus' to binary (assuming "Diabetes" and "No Diabetes" are the levels)
filtered_data$dm <- ifelse(filtered_data$dm == "yes", 1, 0)
filtered_data$insurance <- ifelse(filtered_data$insurance == 1, 1, 0)

# Fit logistic regression model
model <- glm(dm ~ location, data = filtered_data, family = binomial)

# Summary of the model
summary(model)

# Extract the coefficient for Louisa (assuming Louisa is not the reference category)
log_odds_ratio <- coef(model)["locationLouisa"]

# Print the log odds ratio rounded to two decimal places
round(log_odds_ratio, 2)


# Recode 'DiabetesStatus' to binary (assuming "Diabetes" and "No Diabetes" are the levels)
g$dm <- ifelse(g$dm == "yes", 1, 0)
g$insurance <- as.factor(g$insurance)

# Fit logistic regression model
model <- glm(dm ~ age + chol + insurance, data = g, family = binomial)

# Summary of the model
summary(model)


# Extract coefficients and their standard errors
coefficients <- summary(model)$coefficients


# Calculate odds ratios and confidence intervals
odds_ratios <- exp(coefficients[, "Estimate"])
conf_intervals <- exp(confint(model))

# Check p-values for statistical significance
p_values <- coefficients[, "Pr(>|z|)"]

# Create a summary table
results <- data.frame(
  Predictor = rownames(coefficients),
  Odds_Ratio = round(odds_ratios, 2),
  `2.5 % CI` = round(conf_intervals[, 1], 2),
  `97.5 % CI` = round(conf_intervals[, 2], 2),
  P_Value = p_values,
  Significant = ifelse(p_values < 0.05, "Yes", "No")
)

# Print the results
print(results)

