install.packages("tidyverse")
library("tidyverse")
library("readxl")

sale_data <- read_xlsx("sale_data.xlsx")

glimpse(sale_data)


new_data <- sale_data %>%
  mutate(Recency = difftime("2008-12-31",sale_data$date , units = "days")) %>%
  group_by(customer_id) %>%
  mutate(Monetary = sum(amount * single_price)) %>% 
  distinct(customer_id, .keep_all= TRUE)

view(new_data)
