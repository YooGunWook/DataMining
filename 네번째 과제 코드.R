
# tree libraries
library(rpart)
#install.packages("rpart.plot",repos="http://healthstat.snu.ac.kr/CRAN/")
library(rpart.plot)

# 1. Training 데이터와 Test 데이터를 50:50의 비율로 분할하시오.(단, 시드번호는 학번의 뒤자리수 4개를 사용하시오)
# Training 데이터와 Test 데이터를 50:50의 비율로 분할하시오. (단, 시드번호는 학번의 뒤자리수 4개를 사용하시오)
setwd('/Volumes/GoogleDrive/내 드라이브/학교 수업/20-1학기/데이터마이닝/')

df = read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')
summary(df)
head(df,10)

# 우선 전처리부터 진행 
# no internet service가 있는 변수들이 존재한다. 굳이 3개의 범주로 나눌 필요 없다고 판단되기 때문에 no로 바꿔준다. 

df[df['MultipleLines'] == 'No phone service',]['MultipleLines'] = 'No'
df[df['OnlineSecurity'] == 'No internet service',]['OnlineSecurity'] = 'No'
df[df['OnlineBackup'] == 'No internet service',]['OnlineBackup'] = 'No'
df[df['DeviceProtection'] == 'No internet service',]['DeviceProtection'] = 'No'
df[df['TechSupport'] == 'No internet service',]['TechSupport'] = 'No'
df[df['StreamingTV'] == 'No internet service',]['StreamingTV'] = 'No'
df[df['StreamingMovies'] == 'No internet service',]['StreamingMovies'] = 'No'


# 둘의 개수 확인 
length(which(df$Churn == 'Yes'))
length(which(df$Churn == 'No'))



set.seed(2073)
nobs=nrow(df)
i = sample(1:nobs, round(nobs*0.5)) # 50% for training data, 50% for test data
train = df[i,] 
test = df[-i,]
nrow(train);nrow(test)

# 2. R 프로그램의 ‘rpart’ 명령어를 사용하여 의사결정나무를 수행하고자 한다. 
# 단, hyper-parameter는 아래와 같이 조정한다.
# A. minsplit = 1 ~ 46 (5의 간격으로)
# B. cp = 0.001 ~ 0.01 (0.001의 간격으로)
# C. xval = 0 으로 고정 (pruning 없음)
# D. 그외 parameter 값들은 default 값을 사용

# id는 빼준다. 
train = subset(train, select = -c(customerID))

# for문을 이용해서 100가지 경우의 수를 만들어준다.
minsplit = seq(from = 1, to = 46, by = 5)
cp = seq(from = 0.001, to = 0.01, by = 0.001)

# 3. 위 2번의 조건에 맞는 의사결정나무를 training 데이터를 이용하여 생성하고, test 데이터를 이용하여 예측 정확도를 계산하고자 한다.
# 이때 예측정확도는 AUROC 값을 사용한다.
library(pROC)

# test도 위와 같은 과정을 해준다. 
test = subset(test, select = -c(customerID))



d = c()
for (i in 1:10) {
  for (j in 1:10) {
    set.seed(2073)
    my.control <- rpart.control(xval=0, cp=cp[j], minsplit=minsplit[i])
    training =  rpart(Churn ~ ., data = train, method="class", control=my.control) 
    prob =  predict(training, newdata=test, type="prob")
    auc_score = auc(roc(test$Churn ~ prob[,2]))
    d = c(d, auc_score)
  }
}

# 4. 3번의 결과, 총 100개의 AUROC 값을 구할 수 있다. 
# 이를 minsplit과 cp 값의 조합에 따라 AUROC 값으로 3차원 포물선 그래프를 생성하시오. (3D surface plot)

library(scatterplot3d)

auc_score = matrix(d,nrow=length(cp),ncol=length(minsplit))
auc_score


persp(minsplit,cp,auc_score, theta = 30, phi = 30, expand = 0.8, col = "lightblue",ticktype = 'detailed')

my.control <- rpart.control(xval=0, cp=cp[10], minsplit=minsplit[1])
training =  rpart(Churn ~ ., data = train, method="class", control=my.control)
prp(training, type=4, extra=2, digits=3)
