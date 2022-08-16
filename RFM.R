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


pre_RFM <- com_data %>%
  mutate(final_R = cut_number(com_data$Recency, n = 5, label = FALSE, binType = "explicit")) %>% 
  mutate(final_F = cut_number(com_data$Frequency, n = 5, label = FALSE, binType = "explicit")) %>%
  mutate(final_M = cut_number(com_data$Monetary, n = 5, label = FALSE, binType = "explicit")) 

glimpse(pre_RFM)
view(pre_RFM)

pre_RFM$final_R = 100*(6-pre_RFM$final_R)
pre_RFM$final_F = 10*pre_RFM$final_F

glimpse(pre_RFM)

final_RFM <- pre_RFM %>%
  mutate(RFM = pre_RFM$final_R+pre_RFM$final_F+pre_RFM$final_M)

glimpse(final_RFM)
View(final_RFM)

write_csv(final_RFM, "RFM.csv")
