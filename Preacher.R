install.packages("data.table") 
library(data.table)
library(dplyr)
library(glmnet)
library(readr)

## Load data in csv files
trainfile=fread("train.csv", sep = ",", header= TRUE)
transactionfile=fread("transactions.csv", sep = ",", header= TRUE) 
## merge trainfile and transactionfile by "msno" -- Isha
Data = merge(trainfile, transactionfile, by = "msno", all = FALSE)

## train the MODEL
MyTrainModel = glm(is_churn ~ transaction_date+plan_list_price +
                     payment_method_id+ payment_plan_days+actual_amount_paid+
                     is_auto_renew + membership_expire_date + 
                     is_cancel, family=binomial(link='logit'), data = Data)

## Working on kaggle submission dataset
kaggleData = fread("sample_submission_v2.csv", sep = ",", header= TRUE) 
transaction2 = fread("transactions_v2.csv", sep = ",", header= TRUE) 
transaction2 = rbind(transaction2,transaction)
DataKG = merge(kaggleData, transaction2, by = "msno", all = FALSE)
#Prediction 
kgPred = predict(MyTrainModel, type='response', newdata = DataKG)
dataOutput = data.frame(msno=DataKG$msno,is_churn=kgPred)
dataOutput = dataOutput %>% select(msno,is_churn) %>% group_by(msno) %>% summarise(is_churn = mean(is_churn))
write.csv(dataOutput, "submission_Preacher.csv",row.names = FALSE)
