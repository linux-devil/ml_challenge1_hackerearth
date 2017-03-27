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



train[,c("emp_title","zip_code","addr_state") := NULL]
write.csv(train,"train_mod.csv",row.names=FALSE)
write.csv(test,"test_mod.csv",row.names=FALSE)


cat_cols = ['grade', 'sub_grade', 'emp_length', 'home_ownership', 'verification_status', 'purpose', 'initial_list_status']
'''
one hot encoding around :
initial_list_status
purpose
verification_status
home_ownership
grade
sub_grade
'''

import numpy as np
import pandas as pd
data = pd.read_csv('train_mod.csv')
data = data.drop('loan_status', axis=1)
data = data.fillna("0")
test_data = pd.read_csv('test_mod.csv')
test_data = test_data.fillna("0")
num_train = np.shape(data)[0]
data = pd.concat([data,test_data],ignore_index=True)
data = data.fillna("0")
data['last_week_pay'] = data['last_week_pay'].str.extract('(\d+)')
data = data.fillna("0")
data['last_week_pay'] = data['last_week_pay'].astype(int)
#data['term'] = data['term'].str.extract('(\d+)')
data = data.fillna("0")
data['term'] = data['term'].astype(int)

from sklearn.preprocessing import LabelEncoder
le = {}

for col in cat_cols:
    le[col] = LabelEncoder()
    data[col] = le[col].fit_transform(data[col])
    le[col].classes_ = np.append(le[col].classes_, 'other')
    print('Encoded: ', col)

one_hot = pd.get_dummies(data['initial_list_status'])
data = data.drop('initial_list_status', axis=1)
data = data.join(one_hot)
one_hot = pd.get_dummies(data['purpose'])
data = data.drop('purpose', axis=1)
data = data.join(one_hot,lsuffix="_purpose")
one_hot = pd.get_dummies(data['verification_status'])
data = data.drop('verification_status', axis=1)
data = data.join(one_hot,lsuffix="_verification_status")
one_hot = pd.get_dummies(data['home_ownership'])
data = data.drop('home_ownership', axis=1)
data = data.join(one_hot,lsuffix="_home_ownership")
one_hot = pd.get_dummies(data['grade'])
data = data.drop('grade', axis=1)
data = data.join(one_hot,lsuffix="_grade")
one_hot = pd.get_dummies(data['sub_grade'])
data = data.drop('sub_grade', axis=1)
data = data.join(one_hot,lsuffix="_sub_grade")
data = data.fillna("0")
data = data.drop('member_id',axis=1)

X = data[:num_train]
X_test = data[num_train:]

pq = pd.read_csv('train_mod.csv')
label = pq['loan_status']

from sklearn.ensemble import RandomForestClassifier

rf = RandomForestClassifier(n_estimators=100, verbose=5, n_jobs=-1)
rf.fit(X,label)
preds = rf.predict_proba(X_test)

data_test = pd.read_csv("test_mod.csv")
rows = data_test['member_id'].copy()
