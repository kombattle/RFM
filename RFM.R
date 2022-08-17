###install packages and library to read xlsx file

install.packages("tidyverse")
library("tidyverse")
library("readxl")

sale_data <- read_xlsx("sale_data.xlsx")

glimpse(sale_data)

###tranform date diff to amount of day and sum the amount of price 
new_data <- sale_data %>%
  mutate(Recency = difftime("2008-12-31",sale_data$date , units = "days")) %>%
  group_by(customer_id) %>%
  mutate(Monetary = sum(amount * single_price)) %>% 
  distinct(customer_id, .keep_all= TRUE)

view(new_data)

###count the frequency amount
fr_data <- sale_data %>%
  count(customer_id)


###join data together
com_data <- fr_data %>%
  left_join(new_data,by = "customer_id") %>%
  select(customer_id, Recency, Frequency = n,Monetary )


###clean data Recency to be integer and round number of Monetary
com_data$Recency <- gsub("[a-z]$","",com_data$Recency) 
com_data$Recency <- as.integer(com_data$Recency)
com_data$Monetary <- round(com_data$Monetary,2)
glimpse(com_data)

view(com_data)

summary(com_data)

###equal the group of data to 1-5 by arrage
pre_RFM <- com_data %>%
  mutate(final_R = cut_number(com_data$Recency, n = 5, label = FALSE, binType = "explicit")) %>% 
  mutate(final_F = cut_number(com_data$Frequency, n = 5, label = FALSE, binType = "explicit")) %>%
  mutate(final_M = cut_number(com_data$Monetary, n = 5, label = FALSE, binType = "explicit")) 

glimpse(pre_RFM)
view(pre_RFM)

###make number to use with RFM model
pre_RFM$final_R = 100*(6-pre_RFM$final_R)
pre_RFM$final_F = 10*pre_RFM$final_F

glimpse(pre_RFM)+


###create columns to make final columns of RFM model
final_RFM <- pre_RFM %>%
  mutate(RFM = pre_RFM$final_R+pre_RFM$final_F+pre_RFM$final_M)

glimpse(final_RFM)
View(final_RFM)

write_csv(final_RFM, "RFM.csv")
