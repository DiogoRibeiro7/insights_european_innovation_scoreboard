library(readxl)
library(openxlsx)
library(tidyr)
library(dplyr)
library(stringr)


# Import the full dataset
dados <- read_excel("EIS_Data.xlsx")
print(colnames(dados))

# Remove by column name
dados <- dados[, -which(names(dados) %in% c("Column1", "Column2", "Region", "RegionName"))]  # Removes columns 'Age' and 'Height'


# dados <- dados %>%
#   mutate(Indicator = str_remove(Indicator, "^[0-9\\.]+\\s+"))

# dados <- dados %>%
#   mutate(
#     Numbers = str_extract(Indicator, "^[0-9\\.]+"),
#     Indicator = str_remove(Indicator, "^[0-9\\.]+\\s+")
#   )

View(dados)
print(colnames(dados))
# # Use pivot_wider to reshape the data
# wide_data <- dados %>%
#   pivot_wider(
#     names_from = Indicator,  # Names for new columns come from the 'Innovation' column
#     values_from = Value       # Values for these columns come from the 'Value' column
#   )

# Use pivot_wider to reshape the data
wide_data <- dados %>%
  pivot_wider(
    names_from = Indicator,  # Names for new columns come from the 'Indicator' column
    values_from = Value,     # Values for these columns come from the 'Value' column
    id_cols = c(Year, Country, CountryName, Perf, Level, Zone)  # Identify rows by 'Country' and 'Year'
  )

View(wide_data)

# Optional: Write the combined data to a new Excel file
write.xlsx(wide_data, "EIS_Data_transformed.xlsx")

