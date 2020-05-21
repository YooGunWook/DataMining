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

df_final$bathrooms = round(df_final$bathrooms)

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

#df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & is.na(df_final$bedrooms),]$bedrooms = 
 # mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & !is.na(df_final$bedrooms),]$bedrooms)

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bedrooms),]$bedrooms = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bedrooms),]$bedrooms)

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & is.na(df_final$bedrooms),]$bedrooms = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & !is.na(df_final$bedrooms),]$bedrooms)

#df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & is.na(df_final$bedrooms),]$bedrooms = 
 #mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & !is.na(df_final$bedrooms),]$bedrooms)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bedrooms),]$bedrooms = 
  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bedrooms),]$bedrooms)

#df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & is.na(df_final$bedrooms),]$bedrooms = 
 # mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & !is.na(df_final$bedrooms),]$bedrooms)

#df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & is.na(df_final$bedrooms),]$bedrooms = 
 #mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & !is.na(df_final$bedrooms),]$bedrooms)

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & is.na(df_final$bedrooms),]$bedrooms = 
  mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$bedrooms),]$bedrooms)

#df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & is.na(df_final$bedrooms),]$bedrooms = 
 #mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & !is.na(df_final$bedrooms),]$bedrooms)

df_final$bedrooms = round(df_final$bedrooms)

# bed
bed = df_final %>%filter(!is.na(beds)) %>% group_by(property_type,room_type) %>%
  summarize(mean_bed = mean(beds))
bed

#df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & is.na(df_final$beds),]$beds = 
#  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Shared room' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'Apartment' & df_final$room_type == 'Private room' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Shared room' & !is.na(df_final$beds),]$beds)

# df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & is.na(df_final$beds),]$beds = 
#  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'House' & df_final$room_type == 'Private room' & !is.na(df_final$beds),]$beds)

#df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & is.na(df_final$beds),]$beds = 
#  mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Shared room' & !is.na(df_final$beds),]$beds)

#df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & is.na(df_final$beds),]$beds = 
#  mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Entire home/apt' & !is.na(df_final$beds),]$beds)

df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & is.na(df_final$beds),]$beds = 
  mean(df_final[df_final$property_type == 'Other' & df_final$room_type == 'Private room' & !is.na(df_final$beds),]$beds)

df_final$beds = round(df_final$beds)


# 결측치 확인
colSums(is.na(df_final))
length(which(df_final$host_has_profile_pic == 't'))
length(which(df_final$host_has_profile_pic == 'f'))
length(which(df_final$host_identity_verified == 't'))
length(which(df_final$host_identity_verified == 'f'))
 
df_final[is.na(df_final$host_has_profile_pic),]$host_has_profile_pic = 't'
df_final[is.na(df_final$host_identity_verified),]$host_identity_verified = 't'
df_final[is.na(df_final$host_response_rate),]$host_response_rate = mean(df_final[!is.na(df_final$host_response_rate),]$host_response_rate)
df_final_real = df_final[!is.na(df_final$zipcode),]
colSums(is.na(df_final_real))


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
summary(df_final_real)
plot(df_final$price_ratio~df_final$review_scores_rating)

library(lattice)
mypanel <- function(x, y) {
  panel.xyplot(x, y)
  panel.loess(x, y, col="red", lwd=2, lty=2)
}


# 최종모형
lm_df<-lm(log(price_ratio) ~ property_type + bed_type  + room_type + accommodates + log(review_scores_rating) + log(number_of_reviews)
          + city + bathrooms + bedrooms + beds + cancellation_policy + cleaning_fee + host_identity_verified + cancellation_policy + instant_bookable
          + property_type:log(review_scores_rating) + host_response_rate + city:host_response_rate
          + cleaning_fee:log(review_scores_rating) + city:log(review_scores_rating) + room_type:log(review_scores_rating)
          + city:log(number_of_reviews) + room_type:accommodates  + city:bathrooms
          + property_type:bathrooms + room_type:bathrooms + room_type:bedrooms + host_identity_verified:bathrooms + instant_bookable:bathrooms
          + property_type:bedrooms + city:bedrooms + city:beds + review_scores_rating:bedrooms_size + review_scores_rating:bathrooms_size
          + log(number_of_reviews):bedrooms_size + log(number_of_reviews):bathrooms_size +bathrooms:bedrooms + zipcode, data=df_final_real)

summary(lm_df)


length(unique(df_final$zipcode))

# 이상치 탐색 
out_out = outlierTest(lm_df)
out_out
# 이상치 지우기
df_final1 = df_final_real[-c(18450,25454,19337,11459,26100,1869,12592,24082,27003,4574),]
df_final2 = df_final1[-c(25247,26775,10912,17804,28541,8156,24670,14326,14565,20308),]
df_final3 = df_final2[-c(21544,3985,19657,17995,8156,26359,24828,17470,14010,16886),]
df_final4 = df_final3[-c(21689,16284,24953,23414,7190,8361,9244,16121,14125,14260),]
df_final5 = df_final4[-c(7283,7809,23925,2594,10313,10550,3623,26221,1795,28566),]
df_final6 = df_final5[-c(27115,13617,1188,28674,4997,17004,15125,7889,3452,6498),]
df_final7 = df_final6[-c(7071,9617,12792,15387,26816,19286),]
df_final8 = df_final7[-c(22536),]


# 잔차를 통해 비선형인것을 파악함. 따라서 y값에 log 변환 취함. 
# 비선형이기 때문에 log변환을 통해 선형으로 바꿔준다. 

plot(lm_df,which=2)
plot(lm_df,which=1)


# step_wise
# 생각보다 유의미한 결과가 안나와서 생략
step_wise = step(lm_df, direction = 'both')
summary(step_wise)


## 결과는 나쁘지 않지만, 교호 작용을 통해 좀 더 성능을 높여보려고 한다. 
## 위의 결과로 넘어가게 된다. 
# review_score_rating과 property_type, bed_type간에 어느정도 차이가 있는 것을 볼 수 있다. 이 변수를 교호작용
xyplot(log(price_ratio)~review_scores_rating|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|city,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|cancellation_policy,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~number_of_reviews|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|city,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|cancellation_policy,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~accommodates|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|city,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~accommodates|cancellation_policy,data=df_final,panel=mypanel)

xyplot(log(price_ratio)~host_response_rate|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|city,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|cancellation_policy,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~bathrooms|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|city,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bathrooms|cancellation_policy,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~bedrooms|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|city,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~bedrooms|cancellation_policy,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~beds|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|city,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~beds|cancellation_policy,data=df_final,panel=mypanel)


xyplot(log(price_ratio)~log_price|property_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|bed_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|room_type,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|cleaning_fee,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|host_identity_verified,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|instant_bookable,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|city,data=df_final,panel=mypanel)
xyplot(log(price_ratio)~log_price|cancellation_policy,data=df_final,panel=mypanel)


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

df_final_real$bathrooms_size = df_final_real$bathrooms >= 2
df_final_real$bedrooms_size = df_final_real$bedrooms >= 2


# 의미있는 교호작용 찾아보자
xyplot(log(price_ratio)~review_scores_rating|bathrooms_size,data=df_final_real,panel=mypanel)
xyplot(log(price_ratio)~review_scores_rating|bedrooms_size,data=df_final_real,panel=mypanel)

xyplot(log(price_ratio)~number_of_reviews|bathrooms_size,data=df_final_real,panel=mypanel)
xyplot(log(price_ratio)~number_of_reviews|bedrooms_size,data=df_final_real,panel=mypanel)

xyplot(log(price_ratio)~accommodates|bath_rooms_size,data=df_final_real,panel=mypanel)
xyplot(log(price_ratio)~accommodates|bedrooms_size,data=df_final_real,panel=mypanel)

xyplot(log(price_ratio)~host_response_rate|bathrooms_size,data=df_final_real,panel=mypanel)
xyplot(log(price_ratio)~host_response_rate|bedrooms_size,data=df_final_real,panel=mypanel)


# 한쪽으로 치우쳐진 데이터가 있는지 확인.
hist(log(df_final3$price_ratio))
hist(df_final3$host_response_rate)
hist(df_final3$accommodates)
hist(log(df_final3$review_scores_rating))
hist(log(df_final3$number_of_reviews))
hist(df_final$bathrooms)
hist(df_final$bedrooms)
hist(log(df_final$beds))

# 예측

set.seed(1234)
i = sample(1:nobs, round(nobs*0.7)) #70% for training data, 40% for testdata
nobs=nrow(df_final_real)
train = df_final_real[i,] 
test = df_final_real[-i,]
pred1 = predict(lm_df, newdata=test, type='response')
pred2 = predict(step_wise, newdata=test, type='response')
unique(df_final$zipcode)
pred1
pred2

cor(test$price_ratio, exp(pred1))^2
cor(log(test$price_ratio), pred2)^2
mean(abs(test$price_ratio - exp(pred1))/abs(test$price_ratio))*100
mean(abs(log(test$price_ratio) - pred2)/abs(log(test$price_ratio)))*100
