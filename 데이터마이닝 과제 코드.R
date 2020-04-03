# 경로 설정
library(readxl)
library(dplyr)
library(tidyverse)
library(magrittr)
library(car)
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
a
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

bathroom = df_final %>%filter(!is.na(bathrooms)) %>% group_by(property_type,room_type) %>%
  summarize(mean_bath = mean(bathrooms))
bathroom

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & is.na(df_final$bathrooms),]$bathrooms = 
  round(mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & !is.na(df_final$bathrooms),]$bathrooms))

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bathrooms),]$bathrooms = 
  round(mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bathrooms),]$bathrooms))

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & is.na(df_final$bathrooms),]$bathrooms = 
  round(mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & !is.na(df_final$bathrooms),]$bathrooms))

df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bathrooms),]$bathrooms = 
  round(mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bathrooms),]$bathrooms))

df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & is.na(df_final$bathrooms),]$bathrooms = 
  round(mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & !is.na(df_final$bathrooms),]$bathrooms))

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & is.na(df_final$bathrooms),]$bathrooms = 
  round(mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & !is.na(df_final$bathrooms),]$bathrooms))

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bathrooms),]$bathrooms = 
  round(mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bathrooms),]$bathrooms))

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & is.na(df_final$bathrooms),]$bathrooms = 
  round(mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & !is.na(df_final$bathrooms),]$bathrooms))

# review_score
review_score = df_final %>%filter(!is.na(review_scores_rating)) %>% group_by(property_type,room_type) %>%
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
bedroom = df_final %>%filter(!is.na(bedrooms)) %>% group_by(property_type,room_type) %>%
  summarize(mean_bedrooms = mean(bedrooms))
bedroom

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & is.na(df_final$bedrooms),]$bedrooms = 
  round(mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & !is.na(df_final$bedrooms),]$bedrooms))

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bedrooms),]$bedrooms = 
  round(mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bedrooms),]$bedrooms))

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & is.na(df_final$bedrooms),]$bedrooms = 
  round(mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & !is.na(df_final$bedrooms),]$bedrooms))

df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & is.na(df_final$bedrooms),]$bedrooms = 
 round(mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & !is.na(df_final$bedrooms),]$bedrooms))

df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bedrooms),]$bedrooms = 
  round(mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bedrooms),]$bedrooms))

df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & is.na(df_final$bedrooms),]$bedrooms = 
  round(mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & !is.na(df_final$bedrooms),]$bedrooms))

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & is.na(df_final$bedrooms),]$bedrooms = 
 round(mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & !is.na(df_final$bedrooms),]$bedrooms))

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bedrooms),]$bedrooms = 
  round(mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bedrooms),]$bedrooms))

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & is.na(df_final$bedrooms),]$bedrooms = 
 round(mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & !is.na(df_final$bedrooms),]$bedrooms))


# bed
bed = df_final %>%filter(!is.na(beds)) %>% group_by(property_type,room_type) %>%
  summarize(mean_bed = mean(beds))
bed

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & is.na(df_final$beds),]$beds = 
  round(mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & !is.na(df_final$beds),]$beds))

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & is.na(df_final$beds),]$beds = 
  round(mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$beds),]$beds))

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & is.na(df_final$beds),]$beds = 
  round(mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & !is.na(df_final$beds),]$beds))

df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & is.na(df_final$beds),]$beds = 
  round(mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & !is.na(df_final$beds),]$beds))

df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & is.na(df_final$beds),]$beds = 
  round(mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$beds),]$beds))

df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & is.na(df_final$beds),]$beds = 
  round(mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & !is.na(df_final$beds),]$beds))

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & is.na(df_final$beds),]$beds = 
  round(mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & !is.na(df_final$beds),]$beds))

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & is.na(df_final$beds),]$beds = 
  round(mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$beds),]$beds))

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & is.na(df_final$beds),]$beds = 
  round(mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & !is.na(df_final$beds),]$beds))



# 결측치 확인
colSums(is.na(df_final))
length(which(df_final$host_has_profile_pic == 't'))
length(which(df_final$host_has_profile_pic == 'f'))
length(which(df_final$host_identity_verified == 't'))
length(which(df_final$host_identity_verified == 'f'))
 
df_final[is.na(df_final$host_has_profile_pic),]$host_has_profile_pic = 't'
df_final[is.na(df_final$host_identity_verified),]$host_identity_verified = 't'
# df_final[is.na(df_final$host_response_rate),]$host_response_rate = median(df_final[!is.na(df_final$host_response_rate),]$host_response_rate)



response_rate = df_final %>%filter(!is.na(host_response_rate)) %>% group_by() %>%
  summarize(response_rate1 = mean(host_response_rate))
response_rate


boxplot(df_final$property_type, df_final$host_response_rate)

unique(df_final$host_response_rate)

unique(df_final$host_has_profile_pic)

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

lm_df<-lm(log(price_ratio) ~ property_type + bed_type  + room_type + accommodates + review_scores_rating + number_of_reviews
          + city + bathrooms + bedrooms + beds + cancellation_policy + cleaning_fee + host_identity_verified + instant_bookable
          + property_type:review_scores_rating + host_response_rate + city:host_response_rate
          + cleaning_fee:review_scores_rating + city:review_scores_rating + room_type:review_scores_rating
          + city:number_of_reviews + room_type:accommodates  + city:bathrooms
          + property_type:bathrooms + room_type:bathrooms + room_type:bedrooms + host_identity_verified:bathrooms + instant_bookable:bathrooms
          + property_type:bedrooms + city:bedrooms + city:beds + review_scores_rating:bedrooms_size + review_scores_rating:bathrooms_size
          + number_of_reviews:bedrooms_size + number_of_reviews:bathrooms_size +bathrooms:bedrooms, data=df_final3)


summary(lm_df)
summary(lm_df_price)


# 이상치 탐색 
out_out = outlierTest(lm_df)
out_out
# 이상치 지우기
df_final1 = df_final[-c(12681,26319,19495,11540,25467,1882,28790,7952,21740,19827),]
df_final2 = df_final1[-c(20469,17611,17018,7244,8212,4604,23624,27001,18288,16409),]
df_final3 = df_final2[-c(16243,28825,17149),]

# host_response_rate 결측치 최적 방법 찾아보기
install.packages('mice')
install.packages('Amelia')
library(mice)
library(Amelia)
df_NA = df_final3
temp<- mice(df_NA, maxit = 50, method = 'pmm')
tempdata2 <- amelia(x = df_final3, m = 5)

# 잔차를 통해 비선형인것을 파악함. 따라서 y값에 log 변환 취함. 
# 비선형이기 때문에 log변환을 통해 선형으로 바꿔준다. 
plot(lm_df,which=2)
plot(lm_df,which=1)

boxplot(df_final$log_price)

df_final1 = df_final[df_final$log_price < 7,]
df_final1 = df_final1[df_final1$log_price > 3,]

plot(lm_df_price,which=2)
plot(lm_df_price,which=1)

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
xyplot(log(price_ratio)~number_of_reviews|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|city,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~accommodates|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|city,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~host_response_rate|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|city,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~bathrooms|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|city,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~bedrooms|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|city,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~beds|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|city,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~log_price|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|city,data=df_final,panel=mypanel)


# 잔차를 통해 좀 더 좋은 변수 찾아보기
xyplot(lm_df$residuals~review_scores_rating|property_type,data=df_final,panel=mypanel)
xyplot(lm_df$residuals~review_scores_rating|bed_type,data=df_final,panel=mypanel)
xyplot(lm_df$residuals~review_scores_rating|room_type,data=df_final,panel=mypanel)
xyplot(lm_df$residuals~review_scores_rating|cleaning_fee,data=df_final,panel=mypanel)
xyplot(lm_df$residuals~review_scores_rating|host_identity_verified,data=df_final,panel=mypanel)
xyplot(lm_df$residuals~review_scores_rating|instant_bookable,data=df_final,panel=mypanel)
xyplot(lm_df$residuals~review_scores_rating|city,data=df_final,panel=mypanel)

xyplot(lm_df$residuals~number_of_reviews|property_type,data=df_final,panel=mypanel)
xyplot(lm_df$residuals~number_of_reviews|bed_type,data=df_final,panel=mypanel)
xyplot(lm_df$residuals~number_of_reviews|room_type,data=df_final,panel=mypanel)
xyplot(lm_df$residuals~number_of_reviews|cleaning_fee,data=df_final,panel=mypanel)
xyplot(lm_df$residuals~number_of_reviews|host_identity_verified,data=df_final,panel=mypanel)
xyplot(lm_df$residuals~number_of_reviews|instant_bookable,data=df_final,panel=mypanel)
xyplot(lm_df$residuals~number_of_reviews|city,data=df_final,panel=mypanel)


xyplot(lm_df_price$residuals~host_response_rate|property_type,data=df_final,panel=mypanel)
xyplot(lm_df_price$residuals~host_response_rate|bed_type,data=df_final,panel=mypanel)
xyplot(lm_df_price$residuals~host_response_rate|room_type,data=df_final,panel=mypanel)
xyplot(lm_df_price$residuals~host_response_rate|cleaning_fee,data=df_final,panel=mypanel)
xyplot(lm_df_price$residuals~host_response_rate|host_identity_verified,data=df_final,panel=mypanel)
xyplot(lm_df_price$residuals~host_response_rate|instant_bookable,data=df_final,panel=mypanel)
xyplot(lm_df_price$residuals~host_response_rate|city,data=df_final,panel=mypanel)

xyplot(log(price_ratio)~host_response_rate|host_identity_verified,data=df_final,panel=mypanel)


# tree 써보자

library(rpart)
library(rpart.plot)

# maximal tree
tree1 = rpart(log(price_ratio) ~ property_type + bed_type  + room_type + accommodates + review_scores_rating + number_of_reviews + host_response_rate +
                city + bathrooms + bedrooms + beds + cancellation_policy + cleaning_fee + host_identity_verified + instant_bookable, data = df_final, method="anova")
prp(tree1, type=4, extra=1, digits=3)

df_final$bathrooms_size = df_final$bathrooms >= 2
df_final$bedrooms_size = df_final$bedrooms >= 2



xyplot(log(price_ratio)~review_scores_rating|bathrooms_size,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|bedrooms_size,data=df_final,panel=mypanel)

xyplot(log(price_ratio)~number_of_reviews|bathrooms_size,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|bedrooms_size,data=df_final,panel=mypanel)

xyplot(log(price_ratio)~accommodates|bath_rooms_size,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|bedrooms_size,data=df_final,panel=mypanel)

xyplot(log(price_ratio)~host_response_rate|bath_rooms_size,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|bedrooms_size,data=df_final,panel=mypanel)

boxplot(df_final$number_of_reviews)
boxplot(df_final$review_scores_rating)
boxplot(df_final$beds)
boxplot(df_final$host_response_rate)

unique(df_final$beds)

summary(df_final)

df_final$bathrooms = round(df_final$bathrooms) 

unique(df_final$host_identity_verified)
colSums(is.na(df_final))



unique(df_final$host_response_rate)
