# Load required libraries
library(readxl)
library(MVN)
library(QuantPsyc)
library(energy)
library(Hmisc)
library(plotly)
# library(psych)
library(ltm)
library(webshot)
library(reticulate)
library(car)
library(cluster)
library(psych)
library(readr)
library(purrr)
library(MASS)
library(reticulate)
library(pcaMethods)

# Set the number of digits to display (setting to 6 or more is usually sufficient to ensure at least 4 decimal places)
options(digits=6)


# Import the full dataset
dados <- read_excel("EIS_Data_restricted.xlsx")

# Get the data type of each column in the dataframe
column_types <- sapply(dados, class)
print(column_types)

# Loop through each column in the dataframe
for (column_name in names(dados)) {
  # Check if the column is a factor
  if (is.factor(dados[[column_name]])) {
    # Convert factor to numeric by first converting to character
    dados[[column_name]] <- as.numeric(as.character(dados[[column_name]]))
  } else if (is.character(dados[[column_name]])) {
    # Try converting from character to numeric directly
    # This will introduce NAs where conversion is not possible
    temp <- as.numeric(dados[[column_name]])
    if (all(is.na(temp)) && any(dados[[column_name]] != "")) {
      message(paste("Conversion failed for column:", column_name, "due to non-numeric values."))
    } else {
      dados[[column_name]] <- temp
    }
  }
}

column_types <- sapply(dados, class)
print(column_types)


# Exclude non-score columns
columns_to_remove <- c("Country", "CountryName", "Perf", "Level", "Zone")
# Safely exclude non-score columns
columns_to_keep <- setdiff(names(dados), columns_to_remove)
dados <- dados[, columns_to_keep, drop = FALSE]

# Checking variance of each column
variances <- sapply(dados, var, na.rm = TRUE)
print(variances)
# Identify columns with zero variance
zero_variance_cols <- names(variances[variances == 0])
print(zero_variance_cols)

# Assuming your data is all numeric by now
cor_matrix <- cor(dados, use = "complete.obs")  # Excludes NA values for calculation
print(cor_matrix)

# Before implementing Probabilistic PCA (PPCA) in R, it's important to verify certain
# assumptions and conditions to ensure that your analysis yields reliable and meaningful
# results. Here are the key assumptions for PPCA and how you can check them in R:
unique_years <- unique(dados$Year)
for (year in unique_years){
  year_data <- dados[dados$Year == year,]
  # Close the current graphics device
  dev.off()
  # Reduce the size of the margins
  par(mar = c(2, 2, 2, 2))  # Bottom, Left, Top, Right
  pairs(year_data, main = "Scatterplot Matrix of Data")
}

check_assumptions <- function(data) {
  print("Checking for outliers...")

  # Identify numeric columns more reliably
  numeric_columns <- sapply(data, is.numeric)

  # Handle no numeric columns found
  if (!any(numeric_columns)) {
    print("No numeric columns for outlier detection.")
  } else {
    # Adjusting the plotting layout to fit the number of numeric columns
    par(mfrow = c(1, sum(numeric_columns)))

    # Loop through each numeric column
    for (i in which(numeric_columns)) {
      boxplot(data[[i]], main = names(data)[i])  # Correctly reference each column by its index
    }

    # Reset the plotting layout
    par(mfrow = c(1, 1))
  }

  # Check normality for numeric columns only
  print("Checking normality...")
  if (any(numeric_columns)) {
    apply(data[, numeric_columns, drop = FALSE], 2, function(x) {
      qqnorm(x)
      qqline(x, col = "red")
    })
  } else {
    print("No numeric columns for normality check.")
  }

  # Missing data analysis
  print("Missing data analysis...")
  print(sapply(data, function(x) sum(is.na(x)) / length(x)))

  # Sample size check
  print("Sample size check...")
  print(dim(data))
}


process_data_for_year <- function(data, year) {
  year_data <- data[data$Year == year,]
  columns_to_remove <- c("Year")
  # Safely exclude non-score columns
  columns_to_keep <- setdiff(names(year_data), columns_to_remove)
  dados_ACP <- year_data[, columns_to_keep, drop = FALSE]
  ppca_result <- pca(dados_ACP, nPcs = 2, method = "ppca")
  print(year)
  print(ppca_result)

  # Get scores (principal component coordinates)
  scores <- ppca_result@scores
  print(scores)

  # Get loadings
  loadings <- ppca_result@loadings
  print(loadings)

  # Plot the scores
  # plot(scores[, 1], scores[, 2], main = "PPCA Score Plot", xlab = "PC1", ylab = "PC2", pch = 19)

  # Check for missing data handling
  completed_data <- completeObs(ppca_result)
  print(summary(completed_data))

  # Basic summary of missing data
  print(sum(is.na(completed_data)))  # Total number of missing values
  print(sapply(completed_data, function(x) sum(is.na(x))/length(x)))  # Proportion of missing data per variable

  check_assumptions(completed_data)
}


# Extract unique years from the data
unique_years <- unique(dados$Year)

# Apply the function to each year
results <- lapply(unique_years, function(year) {
  process_data_for_year(dados, year)
})
