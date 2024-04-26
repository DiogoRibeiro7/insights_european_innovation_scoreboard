library(readxl)
library(openxlsx)
library(tidyr)
library(dplyr)
library(stringr)


# Import the full dataset
dados <- read_excel("EIS_Data.xlsx")
View(dados)
print(colnames(dados))

# Remove by column name
dados <- dados[, -which(names(dados) %in% c("Column1", "Column2", "Region", "RegionName"))]  # Removes columns 'Age' and 'Height'

# Use pivot_wider to reshape the data
wide_data <- dados %>%
  pivot_wider(
    names_from = Indicator,  # Names for new columns come from the 'Innovation' column
    values_from = Value       # Values for these columns come from the 'Value' column
  )

# dados <- dados %>%
#   mutate(Indicator = str_remove(Indicator, "^[0-9\\.]+\\s+"))


View(wide_data)

# Optional: Write the combined data to a new Excel file
write.xlsx(wide_data, "EIS_Data_transformed.xlsx")
