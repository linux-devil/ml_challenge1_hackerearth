library(data.table)
library(h2o)
library(caret)
library(stringr)
library(e1071)

train <- read.csv("train_indessa.csv",na.strings = c(""," ",NA))
test <- read.csv("test_indessa.csv",na.strings = c(""," ",NA))

num_col <- colnames(train)[sapply(train, is.numeric)]
num_col <- num_col[!(num_col %in% c("member_id","loan_status"))]
train <- as.data.table(train)
test <- as.data.table(test)
train[,c("funded_amnt","funded_amnt_inv","collection_recovery_fee") := NULL]
test[,c("funded_amnt","funded_amnt_inv","collection_recovery_fee") := NULL]

train[,term := unlist(str_extract_all(string = term,pattern = "\\d+"))]
test[,term := unlist(str_extract_all(string = term,pattern = "\\d+"))]
train[,term := as.integer(term)]
test[,term := as.integer(term)]

train[emp_length == "n/a", emp_length := -1]
train[emp_length == "< 1 year", emp_length := 0]
train[,emp_length := unlist(str_extract_all(emp_length,pattern = "\\d+"))]
train[,emp_length := as.integer(emp_length)]

test[emp_length == "n/a", emp_length := -1]
test[emp_length == "< 1 year", emp_length := 0]
test[,emp_length := unlist(str_extract_all(emp_length,pattern = "\\d+"))]
test[,emp_length := as.integer(emp_length)]

train[,desc := NULL]
test[,desc := NULL]

train[is.na(annual_inc), annual_inc := mean(train$annual_inc)]
train[,annual_inc := log(annual_inc + 10)]

test[is.na(annual_inc), annual_inc := mean(test$annual_inc)]
test[,annual_inc := log(annual_inc + 10)]

se <- colnames(train)[sapply(train, is.numeric)]
se <- se[!(se %in% c("member_id","loan_status"))]

skew <- sapply(train[,se,with=F], function(x) skewness(x,na.rm = T))
skew <- skew[skew > 2] #filter variables with skewness > 2

train[,(names(skew)) := lapply(.SD, function(x) log(x + 10)), .SDcols = names(skew)]

skew_t <- sapply(test[,se,with=F], function(x) skewness(x,na.rm = T))
skew_t <- skew_t[skew_t > 2]

test[,(names(skew)) := lapply(.SD, function(x) log(x + 10)), .SDcols = names(skew)]
train[,dti := log10(dti + 10)]
test[,dti := log10(dti + 10)]

train[,pymnt_plan := NULL]
test[,pymnt_plan := NULL]

train[,verification_status_joint :=  NULL]
test[,verification_status_joint :=  NULL]

train[,application_type := NULL]
test[,application_type := NULL]

train[,title := NULL]
test[,title := NULL]

train[,batch_enrolled := NULL]
test[,batch_enrolled := NULL]

-----------------------------------------------------------
train[,new_var_2 := log(annual_inc/loan_amnt)]
test[,new_var_2 := log(annual_inc/loan_amnt)]
train[,new_var_3 := total_rec_int + total_rec_late_fee]
test[,new_var_3 := total_rec_int + total_rec_late_fee]
train[,new_var_4 := sqrt(loan_amnt * int_rate)]
test[,new_var_4 := sqrt(loan_amnt * int_rate)]

write.csv(train,"train_mod.csv",row.names=FALSE)
write.csv(test,"test_mod.csv",row.names=FALSE)
