# 경로 설정
library(readxl)
library(dplyr)
library(tidyverse)
library(magrittr)
setwd('/Volumes/GoogleDrive/내 드라이브/학교 수업/20-1학기/데이터마이닝/data')
df = read_excel('airbnb.xlsx', sheet = 'airbnb')
head(df)
length(df$id)

# 필수적으로 설정해야되는 변수부터 전처리
df$exp_price = exp(df$log_price)

# property_type 범주 3개로 변경
v = c()
for (i in df$property_type) {
  if (i == 'House') {
    v = c(v,'House')
  } else if (i == 'Apartment') {
    v = c(v,'Apartment')
  } else {
    v = c(v,'Other')
  }
}
df$property_type = v

# bed_type 범주 3개로 변경
v = c()
for (i in df$bed_type) {
  if (i == 'Real Bed') {
    v = c(v,'Bed')
  } else {
    v = c(v,'Other')
  }
}
df$bed_type = v

# 결과 확인
unique(df$bed_type)
unique(df$property_type)

# 11개 이상인 데이터만 뽑기
df = df[df$number_of_reviews >= 11,]

unique(df$city)
df_LA = df[df$city == 'LA',]
df_SF = df[df$city == 'SF',]
df_NYC = df[df$city == 'NYC',]
df_DC = df[df$city == 'DC',]
df_Chicago = df[df$city == 'Chicago',]
df_Boston = df[df$city == 'Boston',]
a = df %>% group_by(city) %>%
      summarize(mean_city = mean(exp_price))
a

v = c()
for (i in df_LA$exp_price) {
  v = c(v, (i/a$mean_city[4])*100)
}
df_LA$price_ratio = v

v = c()
for (i in df_SF$exp_price) {
  v = c(v, (i/a$mean_city[6])*100)
}
df_SF$price_ratio = v

v = c()
for (i in df_NYC$exp_price) {
  v = c(v, (i/a$mean_city[5])*100)
}
df_NYC$price_ratio = v

v = c()
for (i in df_DC$exp_price) {
  v = c(v, (i/a$mean_city[3])*100)
}
df_DC$price_ratio = v

v = c()
for (i in df_Chicago$exp_price) {
  v = c(v, (i/a$mean_city[2])*100)
}
df_Chicago$price_ratio = v

v = c()
for (i in df_Boston$exp_price) {
  v = c(v, (i/a$mean_city[1])*100)
}
df_Boston$price_ratio = v
df_final = rbind(df_LA,df_SF,df_NYC,df_DC,df_Chicago,df_Boston)
df_final



## 1. ##
mean(df_final$price_ratio)
sd(df_final$price_ratio)


## 2. ##
summary(df_final)
df_final$review_scores_rating
plot(df_final$price_ratio~df_final$review_scores_rating)

library(lattice)
mypanel <- function(x, y) {
  panel.xyplot(x, y)
  panel.loess(x, y, col="red", lwd=2, lty=2)
}

xyplot(price_ratio~review_scores_rating|property_type,data=df_final,panel=mypanel)

lm_df<-lm(price_ratio ~ property_type + bed_type + number_of_reviews + room_type + accommodates + review_scores_rating + number_of_reviews + host_response_rate +
            city + , data=df_final)
summary(lm_df)

unique(df_final$neighbourhood)
