setwd('/Volumes/GoogleDrive/내 드라이브/학교 수업/20-1학기/데이터마이닝/data/')

# 데이터 확인 
df = read.csv('directmail.csv')
length(df$RESPOND)
summary(df)
colSums(is.na(df))
# age, fico, income, married, ownhome에 NA존재
# 우선 NA 값 처리를 해줘야 함
# married, ownhome 여부는 0으로 처리
df[is.na(df$MARRIED),]$MARRIED = 0
df[is.na(df$OWNHOME),]$OWNHOME = 0
# 빈공간이 있음. 최빈값인 M을 넣었음. 
df[df$GENDER == '',]$GENDER = 'M'
# ownhome을 기준으로 나머지 값들 채워줌.
df[df$OWNHOME == 0 & is.na(df$AGE),]$AGE = mean(df[df$OWNHOME == 0 & !is.na(df$AGE),]$AGE)
df[df$OWNHOME == 1 & is.na(df$AGE),]$AGE = mean(df[df$OWNHOME == 1 & !is.na(df$AGE),]$AGE)
df[df$OWNHOME == 0 & is.na(df$FICO),]$FICO = mean(df[df$OWNHOME == 0 & !is.na(df$FICO),]$FICO)
df[df$OWNHOME == 1 & is.na(df$FICO),]$FICO = mean(df[df$OWNHOME == 1 & !is.na(df$FICO),]$FICO)
df[df$OWNHOME == 0 & is.na(df$INCOME),]$INCOME = mean(df[df$OWNHOME == 0 & !is.na(df$INCOME),]$INCOME)
df[df$OWNHOME == 1 & is.na(df$INCOME),]$INCOME = mean(df[df$OWNHOME == 1 & !is.na(df$INCOME),]$INCOME)

# 1. First divide the data into two pieces using 50:50 proportion: one for training and the other for test.
# (Use the last four digit of your ID as the seed number and use 50:50 proportion)
set.seed(2073)
nobs=nrow(df)
i = sample(1:nobs, round(nobs*0.5)) #70% for training data, 30% for testdata
train = df[i,] 
test = df[-i,]
nrow(train);nrow(test)

# 2. Fit the (1)Full Logistic Regression, (2)Stepwise Logistic Regression.
full_model = glm(RESPOND~., family="binomial", data=train)
step_wise_model = step(full_model, direction='both')
prob_pred1 = predict(full_model, newdata=test, type='response')
prob_pred2 = predict(step_wise_model, newdata=test, type='response')

# 3. Draw a (non-cumulative) Lift Chart using R for the test data (use % Response as the Y-axis). (Do not use R packages)

scored_dat_full = cbind(prob_pred1, test$RESPOND)
scored_dat_step = cbind(prob_pred2, test$RESPOND)


scored_dat_full = scored_dat_full[order(-prob_pred1),]
scored_dat_step = scored_dat_step[order(-prob_pred2),]

# non_cumulative lift chart response rate for full
a = (sum(scored_dat_full[(1+500*0):(500 + 500*0),][,2])/500)*100
a

for (i in 1:9) {
  b = (sum(scored_dat_full[(1+500*i):(500 + 500*i),][,2])/500)*100
  a = rbind(a,b)
}
a
a = data.frame(a)
decile = c(0:9)
a = cbind(decile,a)

# non_cumulative lift chart response rate for step
b = (sum(scored_dat_step[(1+500*0):(500 + 500*0),][,2])/500)*100
b

for (i in 1:9) {
  c = (sum(scored_dat_step[(1+500*i):(500 + 500*i),][,2])/500)*100
  b = rbind(b,c)
}
b
b = data.frame(b)
b = cbind(decile,b)
b

par(mfrow=c(1, 1))
# plot을 통해 확인하기 
barplot(height = a$a, names.arg = a$decile, main = 'lift chart for full model',xlab ='Decile', ylab = 'Response(%)')
barplot(height = b$b, names.arg = b$decile, main = 'lift chart for step model',xlab = 'Decile', ylab = 'Response(%)')
plot(a$a, type = 'l', col = 3, main ='lift chart for full model',xlab ='Decile', ylab = 'Response(%)')
lines(b$b, type = 'l', col =2)
legend(7,15,c("full_model","step_model"),lwd=c(1,1),col=c(3,2))

# 4. Draw a (cumulative) Lift Chart using R for the test data (use % Response as the Y-axis). (Do not use R packages)

scored_dat_full = cbind(prob_pred1, test$RESPOND)
scored_dat_step = cbind(prob_pred2, test$RESPOND)

scored_dat_full = scored_dat_full[order(-prob_pred1),]
scored_dat_step = scored_dat_step[order(-prob_pred2),]

table(test$RESPOND)

# cumulative lift chart captured response rate for full
sum_full = sum(scored_dat_full[(1+500*0):(500 + 500*0),][,2])
sum_full
a = (sum_full/381)*100
a

for (i in 1:9) {
  c = sum(scored_dat_full[(1+500*i):(500 + 500*i),][,2])
  sum_full =+ sum_full + c
  b = (sum_full/381)*100
  a = rbind(a,b)
}
a

# cumulative lift chart captured response rate for step
sum_step = sum(scored_dat_step[(1+500*0):(500 + 500*0),][,2])
sum_step
b = (sum_step/381)*100
b

for (i in 1:9) {
  c = sum(scored_dat_step[(1+500*i):(500 + 500*i),][,2])
  sum_step =+ sum_step + c
  d = (sum_step/381)*100
  b = rbind(b,d)
}
b

# plot
barplot(height = a[,1], names.arg = c(0:9), main = 'lift chart for full model',xlab ='Decile', ylab = 'Captured Response(%)')
barplot(height = b[,1], names.arg = c(0:9), main = 'lift chart for step model',xlab = 'Decile', ylab = 'Captured esponse(%)')
plot(a[,1], type = 'l', col = 3, main ='lift chart for full model',xlab ='Decile', ylab = 'Captured Response(%)')
lines(b[,1], type = 'l', col =2)
legend(2,100,c("full_model","step_model"),lwd=c(1,1),col=c(3,2))


# 5. Which model would you choose?
# 둘 다 매우 비슷한 형태를 보이고 있으나, full 모델이 1,2 등급에서 조금 더 급격한 하락을 보이고 있고, 튀는 현상이 없으며,
# 마지막으로 cumulative chart에서 좀 더 완만한 형태를 보이고 있기 때문에 full_model을 채택한다. 

# 6. Using the model that you chose in #5, draw a (cumulative) Profit Chart
    # using R for the validation data under the following conditions.   
    # Fixed cost = $5,000
    # Cost per mailing = $7
    # Profit for each purchase = $100
    # mailing rate는 반응률로 한다. -> 등급별 profit을 확인하기 위해서. 

scored_dat_full = cbind(prob_pred1, test$RESPOND)

scored_dat_full = scored_dat_full[order(-prob_pred1),]

sum_full_profit = (sum(scored_dat_full[(1+500*0):(500 + 500*0),][,2]))
percentage = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
cost = 5000 + (7 * 0.1 * 5000)
income = 95 * 100
profit = income - cost
profit
sum_full_profit
percentage[1]


for (i in 1:9) {
  a = (sum(scored_dat_full[(1+500*i):(500 + 500*i),][,2]))
  sum_full_profit =+ sum_full_profit + a
  b = percentage[i]
  cost = 5000 + (7 * b * 5000)
  income = sum_full_profit * 100
  profit1 = income - cost
  profit = rbind(profit,profit1)
}

barplot(height = profit[,1], names.arg = c(0:9), main = 'Cumulative Profit Chart', xlab = 'Decile', ylab = 'Profit')





