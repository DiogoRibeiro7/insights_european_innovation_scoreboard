library(readxl)
library(dplyr)
library(writexl)

data1 <- read_excel("EIS_DATA.xlsx", sheet = "EIS-INN")
data2 <- read_excel("EIS_DATA.xlsx", sheet = "EIS-STR")

combined_data <- bind_rows(data1, data2)

write_xlsx(combined_data, "combined_data.xlsx")

View(combined_data)
