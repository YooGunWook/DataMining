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


# 도시별로 가격 평균 나타내기 위해 따로 분리한다.
unique(df$city)
df_LA = df[df$city == 'LA',]
df_SF = df[df$city == 'SF',]
df_NYC = df[df$city == 'NYC',]
df_DC = df[df$city == 'DC',]
df_Chicago = df[df$city == 'Chicago',]
df_Boston = df[df$city == 'Boston',]
a = df %>% group_by(city) %>%
      summarize(mean_city = mean(exp_price))

# 각 도시별로 가격비를 구현한 것. 
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

# 최종적으로 데이터를 rbind로 합친다.
df_final = rbind(df_LA,df_SF,df_NYC,df_DC,df_Chicago,df_Boston)
df_final

# 이 부분부터는 결측치를 처리하는 과정인데, 무조건적으로 0이나 평균을 넣는 것을 좋아하지 않음.
# 따라서 이 부분에서 property_type과 room_type을 groupby해서 각각의 평균을 각각의 타입의 결측치에 넣어줬다.
# bathroom
bathroom = df %>%filter(!is.na(bathrooms)) %>% group_by(property_type,room_type) %>%
  summarize(mean_bath = mean(bathrooms))
bathroom

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & is.na(df_final$bathrooms),]$bathrooms = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & !is.na(df_final$bathrooms),]$bathrooms)

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bathrooms),]$bathrooms = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bathrooms),]$bathrooms)

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & is.na(df_final$bathrooms),]$bathrooms = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & !is.na(df_final$bathrooms),]$bathrooms)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bathrooms),]$bathrooms = 
  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bathrooms),]$bathrooms)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & is.na(df_final$bathrooms),]$bathrooms = 
  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & !is.na(df_final$bathrooms),]$bathrooms)

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & is.na(df_final$bathrooms),]$bathrooms = 
  mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & !is.na(df_final$bathrooms),]$bathrooms)

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bathrooms),]$bathrooms = 
  mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bathrooms),]$bathrooms)

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & is.na(df_final$bathrooms),]$bathrooms = 
  mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & !is.na(df_final$bathrooms),]$bathrooms)

# review_score
review_score = df %>%filter(!is.na(review_scores_rating)) %>% group_by(property_type,room_type) %>%
  summarize(mean_score = mean(review_scores_rating))
review_score

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & is.na(df_final$review_scores_rating),]$review_scores_rating = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & !is.na(df_final$review_scores_rating),]$review_scores_rating)

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & is.na(df_final$review_scores_rating),]$review_scores_rating = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$review_scores_rating),]$review_scores_rating)

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & is.na(df_final$review_scores_rating),]$review_scores_rating = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & !is.na(df_final$review_scores_rating),]$review_scores_rating)

#df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & is.na(df_final$review_scores_rating),]$review_scores_rating = 
 # mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & !is.na(df_final$review_scores_rating),]$review_scores_rating)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & is.na(df_final$review_scores_rating),]$review_scores_rating = 
  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$review_scores_rating),]$review_scores_rating)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & is.na(df_final$review_scores_rating),]$review_scores_rating = 
  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & !is.na(df_final$review_scores_rating),]$review_scores_rating)

#df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & is.na(df_final$review_scores_rating),]$review_scores_rating = 
 # mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & !is.na(df_final$review_scores_rating),]$review_scores_rating)

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & is.na(df_final$review_scores_rating),]$review_scores_rating = 
  mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$review_scores_rating),]$review_scores_rating)

#df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & is.na(df_final$review_scores_rating),]$review_scores_rating = 
 # mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & !is.na(df_final$review_scores_rating),]$review_scores_rating)


# bedroom
bedroom = df %>%filter(!is.na(bedrooms)) %>% group_by(property_type,room_type) %>%
  summarize(mean_bedrooms = mean(bedrooms))
bedroom

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & is.na(df_final$bedrooms),]$bedrooms = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & !is.na(df_final$bedrooms),]$bedrooms)

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bedrooms),]$bedrooms = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bedrooms),]$bedrooms)

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & is.na(df_final$bedrooms),]$bedrooms = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & !is.na(df_final$bedrooms),]$bedrooms)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & is.na(df_final$bedrooms),]$bedrooms = 
 mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & !is.na(df_final$bedrooms),]$bedrooms)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bedrooms),]$bedrooms = 
  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bedrooms),]$bedrooms)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & is.na(df_final$bedrooms),]$bedrooms = 
  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & !is.na(df_final$bedrooms),]$bedrooms)

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & is.na(df_final$bedrooms),]$bedrooms = 
 mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & !is.na(df_final$bedrooms),]$bedrooms)

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bedrooms),]$bedrooms = 
  mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bedrooms),]$bedrooms)

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & is.na(df_final$bedrooms),]$bedrooms = 
 mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & !is.na(df_final$bedrooms),]$bedrooms)


# bed
bed = df %>%filter(!is.na(beds)) %>% group_by(property_type,room_type) %>%
  summarize(mean_bed = mean(beds))
bed

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & !is.na(df_final$beds),]$beds)



# 결측치 확인
summary(df_final)

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

xyplot(price_ratio~review_scores_rating|cancellation_policy,data=df_final,panel=mypanel)

lm_df<-lm(log(price_ratio) ~  property_type + bed_type  + room_type + accommodates + review_scores_rating + number_of_reviews + host_response_rate +
            city + bathrooms + bedrooms + beds + cancellation_policy + cleaning_fee + host_identity_verified + instant_bookable + property_type:review_scores_rating
          + cleaning_fee:review_scores_rating + city:review_scores_rating  + room_type:review_scores_rating
          + city:number_of_reviews, data=df_final)
summary(lm_df)
# 잔차를 통해 비선형인것을 파악함. 따라서 y값에 log 변환 취함. 
# 비선형이기 때문에 log변환을 통해 선형으로 바꿔준다. 
plot(lm_df,which=2)
plot(lm_df,which=1)

hist(df_final$bathrooms)

step_wise = step(lm_df, direction = 'both')
summary(step_wise)

## 결과는 나쁘지 않지만, 교호 작용을 통해 좀 더 성능을 높여보려고 한다. 
# review_score_rating과 property_type, bed_type간에 어느정도 차이가 있는 것을 볼 수 있다. 이 변수를 교호작용
xyplot(log(price_ratio)~review_scores_rating|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|city,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~number_of_reviews|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|city,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~number_of_reviews|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|city,data=df_final,panel=mypanel)


boxplot(price_ratio~city ,data=df_final)

summary(df_final)


