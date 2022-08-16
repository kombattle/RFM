install.packages("tidyverse")
library("tidyverse")
library("readxl")

sale_data <- read_xlsx("sale_data.xlsx")

glimpse(sale_data)
