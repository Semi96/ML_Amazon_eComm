library(caret)
library(tidyverse)
library(ggplot2)

rSquared <- function(preds, actuals, sampleMean) {
  1 - sum((preds - actuals)^2)/sum((sampleMean-actuals)^2)
}


data <- read.csv("data/predCassNoChina.csv")

# get year, factorize month
data <- data %>% 
  mutate(Year = as.numeric(substr(Date, nchar(Date)-3,nchar(Date))),
         Month = as.factor(ifelse(nchar(Date) == 9, substr(Date, 1, 2), substr(Date, 1, 1))))

############# GRAIN: MONTH ######################

# simple linear regression
mod <- lm(CassIndex ~ . -Date, data)
summary(mod)

############# GRAIN: QUARTER ####################

data_quarter <- data %>% 
  filter(Year < 2021) %>% 
  mutate(Quarter = ifelse(Month %in% c(1,2,3), 1,
                   ifelse(Month %in% c(4,5,6), 2,
                   ifelse(Month %in% c(7,8,9), 3, 4))),
         BeginOfQ = ifelse(Month %in% c(1,4,7,10),1,0))

data_quarterized <- data_quarter %>% 
  group_by(Year, Quarter) %>% 
  summarise(avgCASS = mean(CassIndex))
data_quarterized$BeginOfQ = 1

data_final <- data_quarterized %>% 
  left_join(data_quarter, by=c("Year", "Quarter", "BeginOfQ")) %>% 
  select(Year, Quarter, OilPrice, ExChUs, USCPI, USPMI, avgCASS)
data_final$Quarter <- as.factor(data_final$Quarter)

data_final_train <- data_final %>% 
  filter(Year < 2020)

data_final_test <- data_final %>% 
  filter(Year == 2020)

qmod <- lm(avgCASS ~ ., data_final_train)
summary(qmod)

testPreds <- predict(qmod, newdata = data_final_test)
trainMean <- mean(data_final_train$avgCASS)
testRsquared <- rSquared(testPreds, data_final_test$avgCASS, trainMean)

write.csv(data_final_train, "data_final_train.csv")
write.csv(data_final_test, "data_final_test.csv")
