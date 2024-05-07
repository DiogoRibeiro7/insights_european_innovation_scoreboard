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
  View(completed_data)
}


# Extract unique years from the data
unique_years <- unique(dados$Year)

# Apply the function to each year
results <- lapply(unique_years, function(year) {
  process_data_for_year(dados, year)
})
