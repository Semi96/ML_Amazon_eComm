library(tidyverse)

alldata_train <- read.csv("data/alldata_train.csv")
alldata_test <- read.csv("data/alldata_test.csv")


unqCust <- alldata_test %>% 
  distinct(UniqueCustomer) %>% 
  arrange(UniqueCustomer)
unqCust$custID <- 1:nrow(unqCust)

alldata_train <- alldata_train %>% 
  left_join(unqCust, by="UniqueCustomer")

alldata_test <- alldata_test %>% 
  left_join(unqCust, by="UniqueCustomer")

write.csv(alldata_train, "data/alldata_train.csv")
write.csv(alldata_test, "data/alldata_test.csv")

## Extra

alldata_train_cust1 <- alldata_train %>%
  filter(custID == 1)

library(ggplot2)
ggplot(alldata_train_cust1) + geom_line(aes(x=Week_Num, y=Population))
ggplot(alldata_train_cust1) + geom_line(aes(x=Week_Num, y=Sales))
ggplot(alldata_train_cust1) + geom_line(aes(x=Week_Num, y=Income))
