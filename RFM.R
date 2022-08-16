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

fr_data <- sale_data %>%
  count(customer_id)

com_data <- fr_data %>%
  left_join(new_data,by = "customer_id") %>%
  select(customer_id, Recency, Frequency = n,Monetary )

com_data$Recency <- gsub("[a-z]$","",com_data$Recency) 
com_data$Recency <- as.integer(com_data$Recency)
com_data$Monetary <- round(com_data$Monetary,2)
glimpse(com_data)

view(com_data)

com_data %>%
  summarise(max_recency = max(Recency),
            max_monetry = max(Monetary),
            max_fr = max(frequency),
            min_fr = min(frequency),
            min_mon = min(Monetary),
            min_re = min(Recency))
