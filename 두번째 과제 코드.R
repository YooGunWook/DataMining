# set directory
setwd('/Volumes/GoogleDrive/내 드라이브/학교 수업/20-1학기/데이터마이닝/data')

# load data
df = read.csv('creditcard.csv')
head(df)
dim(df)

# unbalance data
# 이상치가 부정하지 않은 사람에게 당연히 많을 수 밖에 없다. 따라서 그것을 감안해서 boxplot을 보도록 하자.
a = table(df$Class)
a['1']/(a['0']+a['1'])
a['0']/(a['0']+a['1'])
barplot(a)

# check V1~V28, time and amount

boxplot(formula = Time~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="Time vs Class")

# V1의 경우 부정한 사람이 평균적으로 더 낮은 것을 볼 수 있다. 그러나 부정하지 않은 사람의 경우 대부분 0에 가깝다.
boxplot(formula = V1~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V1 vs Class")

# V2의 경우 부정한 사람은 0보다 조금 높게 평균이 형성되어 있다. 부정하지 않은 사람의 경우 평균이 0에 가깝다.
# 모델에서는 이 변수가 나름 유의미하다고 나왔다. 
boxplot(formula = V2~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V2 vs Class")

# V3의 경우 부정한 사람은 0 이하에 대부분 형성되어 있는 것을 볼 수 있고, 부정하지 않은 사람의 경우 대부분 0에 가깝게 분포해 있다.  
boxplot(formula = V3~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V3 vs Class")

# V4의 경우 부정한 사람은 5애 가깝게 분포되어 있고, 부정하지 않은 사람의 경우 0에 가깝게 분포되어 있다. 특이한 점이 있다면 부정한 사람의 경우 이상치가 없지만, 부정하지 않은 사람은 이상치가 많이 존재하는 것을 볼 수 있다.
# 예상한대로 매우 유의미한 변수로 나왔다.
boxplot(formula = V4~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V4 vs Class")

# V5의 경우 엄청 큰 차이를 보이고 있지는 않지만 부정한 사람이 부정하지 않은 사람보다 조금 더 낮게 형성되어 있다.
# 모델에서는 매우 유의미한 변수로 채택되었다.
boxplot(formula = V5~Class, data = df
        , col=c("yellow","green"),
        ylim = c(-50,10),
        xlab="Class",
        main="V5 vs Class")

# V6의 경우 둘 다 0에 가깝게 분포되어 있는 것을 볼 수 있다.  
boxplot(formula = V6~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        ylim = c(-20,20),
        main="V6 vs Class")

# V7의 경우 부정한 사람들이 좀 더 낮게 형성되어 있는 것을 볼 수 있다. 부정하지 않은 사람은 0에 가깝게 형성되어 있다.
boxplot(formula = V7~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        ylim = c(-40,40),
        main="V7 vs Class")

# V8의 경우 둘이 큰 차이를 보이고 있지는 않지만, 부정한 사람들이 좀 더 넓게 분포하고 있는 것을 볼 수 있다. 
# 모델에서는 매우 유의미하게 나왔다. 아마 분포의 차이가 많이 나서 그런듯 하다.
boxplot(formula = V8~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        ylim = c(-10,10),
        main="V8 vs Class")

# V9의 경우 부정한 사람들이 0보다 낮게 형성되어 있는 것을 볼 수 있다. 부정하지 않은 사람은 0에 가깝게 형성되어 있다.
boxplot(formula = V9~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V9 vs Class")

# V10의 경우 부정한 사람들은 0보다 낮게 형성되어 있다. 부정하지 않은 사람은 0에 가깝게 형성되어 있다.
# 예상한대로 모델에서 유의미한 변수로 채택했다.
boxplot(formula = V10~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V10 vs Class")

# V11의 경우 부정한 사람들은 0보다 크게 형성되어 있고, 5애 더 가깝다. 반명 부정하지 않은 사람은 0에 가깝게 형성되어 있다.
boxplot(formula = V11~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V11 vs Class")

# V12의 경우 부정한 사람들은 -5에 가깝게 형성되어 있다. 반명 부정하지 않은 사람은 0에 가깝게 형성되 있다.
# 예상한대로 모델에서 유의미한 변수로 채택했다. 그러나 분포에 비하면 pvalue가 생각보다 조금 높게 나왔다.
boxplot(formula = V12~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V12 vs Class")

# V13의 경우 둘이 큰 차이를 보이고 있지 않다. 
# 분포에서는 큰 차이를 보이고 있지 않지만 매우 유의미하다고 나왔다.
boxplot(formula = V13~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        ylim = c(-4,4),
        main="V13 vs Class")

# V14의 경우 부정한 사람은 -6~-7에 가깝게 형성되어 있다. 반면 부정하지 않은 사람은 0에 가깝게 형성되어 있다.
# 예상한대로 매우 유의미하게 나왔다.
boxplot(formula = V14~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V14 vs Class")

# V15의 경우 큰 차이를 보이고 있지 않다. 
boxplot(formula = V15~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V15 vs Class")

# V16의 경우 부정한 사람은 -3에 가깝게 형성되어 있다. 부정하지 않은 사람은 0에 가깝게 형성되어 있다. 부정한 사람에게는 이상치가 없지만, 부정하지 않은 사람의 경우 위 아래로 이상치가 존재.
# 예상한대로 매우 유의미하게 나왔다. 
boxplot(formula = V16~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V16 vs Class")

# V17의 경우 부정한 사람은 -5에 가깝게 형성되어 있지만, -10까지도 많은 분포를 가지고 있는 것을 볼 수 있다. 반면 부정하지 않은 사람은 0에 가깝게 형성되어 있다. 이 또한 부정하지 않은 사람에게만 이상치 존재. 
boxplot(formula = V17~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V17 vs Class")

# V18의 경우 부정한 사람들은 0에서 -5 사이에 가장 많이 분포 되어있다. 부정하지 않은 사람은 0에 가깝게 분포하고 있다. 
boxplot(formula = V18~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V18 vs Class")

# V19의 경우 극적인 차이가 보이지 않지만, 부정한 사람들이 좀 더 넓게 분포되어 있는 것을 볼 수 있다. 부정하지 않은 사람은 0에 가깝게 분포되어 있다.
boxplot(formula = V19~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V19 vs Class")

# V20의 부정한 사람들이 좀 더 넓은 범위를 가지고 있다.
# 매우 유의미한 변수로 채택되었다. 
boxplot(formula = V20~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        ylim=c(-5,5),
        main="V20 vs Class")

# V21의 경우 부정한 사람들이 좀 더 많은 분포를 가지고 있다.
# 매우 유의미한 변수로 채택되었다. 범위로 인한 차이가 있던것으로 보여짐.
boxplot(formula = V21~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        ylim = c(-5,5),
        main="V21 vs Class")

# V22의 경우에는 둘 다 거의 비슷하다. 
# 거의 비슷한데... 왜 채택된걸까...
boxplot(formula = V22~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V22 vs Class")

# V23의 경우 거의 비슷하다. 
boxplot(formula = V23~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        ylim = c(-5,5),
        main="V23 vs Class")

# V24의 경우 거의 비슷하다.
# 일단 채택은 됐지만, p-value가 높지 않다.
boxplot(formula = V24~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V24 vs Class")

# V25의 경우 거의 비슷하다.
boxplot(formula = V25~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        ylim = c(-5,5),
        main="V25 vs Class")

# V26의 경우 부정한 사람들이 좀 더 넓게 분포하고 있지만 유의미한 차이는 아니다.
boxplot(formula = V26~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        main="V26 vs Class")

# V27의 경우 부정한 사람들이 더 넓게 분포하고 있기 때문에 꽤 유의미해 보인다.
# 예상대로 유의미하게 나왔다.
boxplot(formula = V27~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        ylim=c(-2,3),
        main="V27 vs Class")

# V28의 경우 부정한 사람들이 더 넓게 분포하고 있기 때문에 꽤 유의미해 보인다.
# 아마도 분포로 인해 뽑힌 거 같은데, 막 그렇다고 엄청 유의미하진 않다. 
boxplot(formula = V28~Class, data = df
        , col=c("yellow","green"),
        xlab="Class",
        ylim=c(-2,2),
        main="V28 vs Class")

# amount의 경우 둘 다 큰 차이가 없다. 단지 부정하지 않은 사람의 경우 이상치가 꽤 높다. 
boxplot(formula = Amount~Class, data = df
        , col=c("yellow","green"),
        ylim=c(0,400),
        xlab="Class",
        main="Amount vs Class")

# 2번
summary(df)

# 3번
nobs=nrow(df)
set.seed(1234)
i = sample(1:nobs, round(nobs*0.7)) #70% for training data, 30% for testdata
train = df[i,] 
test = df[-i,]
unique(train$Class)
unique(test$Class)
table(train$Class)
table(test$Class)



model_first = glm(Class~., family="binomial", data=train)
step_model_for = step(model_first, direction='forward')
step_model_back = step(model_first, direction='backward')
step_model_both = step(model_first, direction='both')

summary(model_first)
summary(step_model_for)
summary(step_model_back)
summary(step_model_both)

# f1 score로 모델 평가해서 최적의 모델을 찾을 것!
library(MLmetrics)
prob_pred1 = predict(model_first, newdata=test, type='response')
prob_pred2 = predict(step_model_for, newdata=test, type='response')
prob_pred3 = predict(step_model_back, newdata=test, type='response')
prob_pred4 = predict(step_model_both, newdata=test, type='response')

pred1 <- ifelse(prob_pred1 < 0.0017, 0, 1)
pred2 <- ifelse(prob_pred2 < 0.0017, 0, 1)
pred3 <- ifelse(prob_pred3 < 0.0017, 0, 1)
pred4 <- ifelse(prob_pred4 < 0.0017, 0, 1)

# forward는 원래 모델과 큰 차이 없지만, backward와 both는 약간의 성능 향상이 있었다.
# 여기서 both 모델 채택
F1_Score(y_pred = pred1, y_true = test$Class, positive = "1")
F1_Score(y_pred = pred2, y_true = test$Class, positive = "1")
F1_Score(y_pred = pred3, y_true = test$Class, positive = "1")
F1_Score(y_pred = pred4, y_true = test$Class, positive = "1")


# 4번
# 대부분 예상한대로 변수가 채택되었으나, 몇몇 변수를 보면 큰 차이가 없지만 유의미한 변수가 된 경우도 여럿 별 수 있다. 
# 해석에 대한 지식 부족이거나 이상치에 대한 문제가 있을수도 있다. 이 부분은 따로 확인을 해봐야 될 것 같다. 

# 5번 
# p-value가 0.05 이하인 odds ratio를 해석하려고 한다. 
exp(coef(step_model_both))

# V2가 1 증가하면 부정 사용자일 가능성이 약 1.1배 오르게 된다. 
# V4가 1 증가하면 부정 사용자일 가능성이 약 2.2배 오르게 된다. 
# V5가 1 증가하면 부정 사용자일 가능성이 약 1.2배 오르게 된다. 
# V8가 1 증가하면 부정 사용자일 가능성이 약 0.9배 떨어지게 된다. 
# V10가 1 증가하면 부정 사용자일 가능성이 약 0.42배 떨어지게 된다.
# V12가 1 증가하면 부정 사용자일 가능성이 약 1.2배 오르게 된다. 
# V13가 1 증가하면 부정 사용자일 가능성이 약 0.7배 떨어지게 된다. 
# V14가 1 증가하면 부정 사용자일 가능성이 약 0.6배 떨어지게 된다.
# V16가 1 증가하면 부정 사용자일 가능성이 약 0.7배 떨어지게 된다.
# V20이 1 증가하면 부정 사용자일 가능성이 약 0.7배 떨어지게 된다.
# V21이 1 증가하면 부정 사용자일 가능성이 약 1.4배 오르게 된다.
# V22이 1 증가하면 부정 사용자일 가능성이 약 1.8배 오르게 된다.
# V27이 1 증가하면 부정 사용자일 가능성이 약 0.6배 떨어지게 된다.
# Amount가 1 증가하면 부정 사용자일 가능성이 약 1.001배 오르게 된다.

# 6번 
three_rd = apply(df, 2, quantile)
three_rd
q3 = three_rd['75%',]
q3 = q3[1:30]
q3 = data.frame(t(q3))

prob_pred5 = predict(step_model_both, newdata=q3, type='response')
prob_pred5








