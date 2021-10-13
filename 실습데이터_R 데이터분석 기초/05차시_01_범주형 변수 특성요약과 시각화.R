######################################################################
# [05차시: 개별변수 요약과 집계] 01. 범주형 변수 특성요약과 시각화
######################################################################
 
# --------------------------------------------------
# 데이터셋 로딩
# --------------------------------------------------

# 플레인텍스트파일 로딩
# --------------------------------------------------
# utils::read.csv()함수로 플레인텍스트파일  로딩
setwd("c:/Rtest")
my <- read.csv(file = 'tour.csv', 
               header = TRUE, sep = ',',
               stringsAsFactors = TRUE, 
               strip.white = TRUE,
               na.strings = c('.', '?', 'NA'))

# 전체 데이터셋 특성파악
my
head(my)
class(my)
str(my)


# --------------------------------------------------
# 범주형 변수컬럼 별도 데이터셋으로 추출
# --------------------------------------------------

# 범주형 변수컬럼명 파악
ctg_names <- c('month', 'gender', 'edu', 'job', 
               'age', 'nat', 'other', 'object', 
               'member', 'accom', 'activity')

# 범주형 변수컬럼 데이터셋 추출
ctg <- my[ctg_names]
 

# 범주형 데이터셋 특성파악
# --------------------------------------------------
# 간단 조회
head(ctg, 3)

# 내부구조 조회
str(ctg)

# 기본 기술통계량 파악
summary(ctg)
skim(ctg)


# --------------------------------------------------
# 명목형(이항) 변수컬럼 특성파악
# -------------------------------------------------

# 성별변수 기본특성 파악
# --------------------------------------------------

# 성별 전체내용
ctg$gender

# 성별 일부내용
head(ctg$gender, 10)
tail(ctg$gender, 15)
ctg$gender[10:20]


# 성별 구조파악
length(ctg$gender)
NROW(ctg$gender)
str(ctg$gender)


# 성별 기술통계량
summary(ctg$gender)

library(psych)
psych::describe(ctg$gender)

library(Hmisc)
Hmisc::describe(ctg$gender)

library(skimr)
skim(ctg$gender)


# 성별변수 팩터형변수로 변환
# --------------------------------------------------
ctg$gender_f <- factor(ctg$gender, levels = c(1, 2), 
                       labels = c('M', 'F'))

# 팩터형 변수추가내용 확인
head(ctg)
str(ctg)

# 성별변수 요약집계 
# --------------------------------------------------
# 빈도분석
gender_freq <- table(ctg$gender)
gender_freq

gender_f_freq <- table(ctg$gender_f)
gender_f_freq

# 비율분석
gender_prop <- prop.table(gender_freq)
gender_prop
gender_f_prop <- prop.table(gender_f_freq)
gender_f_prop

# 비율분석 소수자리정리
round(gender_prop, 3)
round(gender_f_prop, 3)

# 백분율로 변환
gender_pect <- round(gender_prop, 3) * 100
gender_pect
gender_f_pect <- round(gender_f_prop, 3) * 100
gender_f_pect


# 성별변수 시각화
# --------------------------------------------------
# barplot() 함수를 이용한 범주형 데이터 그래프

par(mfrow=c(2, 2)) # 멀티 캔버스 프레임 구성

barplot(gender_f_freq,
        main="성별 인원수 분포비교: Simple Bar Plot",
        xlab="성별", ylab="명수")

barplot(gender_f_freq,
        main="성별 인원수 분포비교: Horizontal Bar Plot",
        xlab="성별", ylab="명수",
        horiz=TRUE)

barplot(gender_f_prop,
        main="성별 비율 분포비교: Simple Bar Plot",
        xlab="성별", ylab="명수",
        density=c(20, 30),
        legend=rownames(gender_f_freq))

barplot(gender_f_prop,
        main="성별 비율 분포비교: Horizontal Bar Plot",
        xlab="성별", ylab="명수",
        horiz=TRUE, col=c("lightblue", "pink"), 
        beside=TRUE, legend=rownames(gender_f_freq))

par(mfrow=c(1, 1)) # 멀티 캔버스 프레임 리셋


# --------------------------------------------------
# 명목형(다항) 변수컬럼 특성파악
# -------------------------------------------------

# 방한목적변수 기본특성 파악
# --------------------------------------------------
# 1: 여가·위락·개별휴가, 2: 뷰티·건강·치료, 3. 종교·순례, 4: 쇼핑, 
# 5: 친구·친지 방문, 6: 사업·전문 활동, 7: 교육, 8: 기타

# 방한목적 전체내용
ctg$object

# 방한목적 일부내용
head(ctg$object, 10)
tail(ctg$object, 15)
ctg$object[10:20]

# 방한목적 구조파악
length(ctg$object)
NROW(ctg$object)
str(ctg$object)

# 방한목적 기술통계량
summary(ctg$object)

library(psych)
psych::describe(ctg$object)

library(Hmisc)
Hmisc::describe(ctg$object)

library(skimr)
skim(ctg$object)


# 방한목적변수 팩터형변수로 변환
# --------------------------------------------------
ctg$object_f <- factor(ctg$object, levels = c(1:8), 
                       labels = c('여가·위락·개별휴가', '뷰티·건강·치료', 
                                  '종교·순례', '쇼핑', '친구·친지 방문', 
                                  '사업·전문 활동', '교육', '기타'))

# 팩터형 변수추가내용 확인
head(ctg)
str(ctg)


# 방한목적변수 요약집계 
# --------------------------------------------------
# 빈도분석
object_freq <- table(ctg$object)
object_freq

object_f_freq <- table(ctg$object_f)
object_f_freq

# 빈도분석 결과를 데이터프레임형태로 변환
obj_df <- as.data.frame(object_f_freq)
obj_df
View(obj_df)

# 비율분석
object_prop <- prop.table(object_freq)
object_prop
object_f_prop <- prop.table(object_f_freq)
object_f_prop

# 비율분석 결과를 데이터프레임형태로 변환
obj_prop_df <- as.data.frame(object_f_prop)
obj_prop_df
View(obj_prop_df)

# 비율을 기준으로 데이터프레임 내림차순 정렬
library(dplyr)
arrange(obj_prop_df, desc(Freq))


# 비율분석 소수자리정리
round(object_prop, 3)
round(object_f_prop, 3)

# 백분율분석
object_pect <- round(object_prop, 3) * 100
object_pect
object_f_pect <- round(object_f_prop, 3) * 100
object_f_pect

# 백분율분석 결과를 데이터프레임형태로 변환
obj_pect_df <- as.data.frame(object_f_pect)
obj_pect_df
View(obj_pect_df)


# 방한목적변수 시각화
# --------------------------------------------------
# barplot() 함수를 이용한 범주형 데이터 그래프

par(mfrow=c(2, 2)) # 멀티 캔버스 프레임 구성

barplot(object_f_freq,
        main="방한목적 인원수 분포비교",
        xlab="방한목적", ylab="명수")

barplot(object_f_freq,
        main="방한목적 인원수 분포비교",
        xlab="방한목적", ylab="명수", las = 1, 
        horiz=TRUE)

barplot(object_f_prop,
        main="방한목적 비율 분포비교",
        xlab="방한목적", ylab="명수",
        density=c(20, 30),
        legend=rownames(object_f_freq))

barplot(object_f_prop,
        main="방한목적 비율 분포비교",
        xlab="방한목적", ylab="명수", las = 1,
        horiz=TRUE, col=c("lightblue", "pink"), 
        beside=TRUE, legend=rownames(object_f_freq))

par(mfrow=c(1, 1)) # 멀티 캔버스 프레임 리셋



# --------------------------------------------------
# 서열형형 변수컬럼 특성파악
# -------------------------------------------------

# 연령대변수 기본특성 파악
# --------------------------------------------------
# 0: 15~20세 , 1: 21~30세 , 2: 31~40세, 
# 3: 41~50세,  4: 51~60세,  5: 61세 이상

# 연령대 전체내용
ctg$age

# 연령대 일부내용
head(ctg$age, 10)
tail(ctg$age, 15)
ctg$age[10:20]

# 연령대 구조파악
length(ctg$age)
NROW(ctg$age)
str(ctg$age)

# 연령대 기술통계량
summary(ctg$age)

library(psych)
psych::describe(ctg$age)

library(Hmisc)
Hmisc::describe(ctg$age)

library(skimr)
skim(ctg$age)


# 연령대변수 팩터형변수로 변환
# --------------------------------------------------
ctg$age_f <- factor(ctg$age, levels = c(0, 1, 2, 3, 4, 5), 
                    labels = c('5~20세', '21~30세', '31~40세', 
                               '41~50세', '51~60세', '61세이상'))

# 팩터형 변수추가내용 확인
head(ctg)
str(ctg)


# 연령대변수 요약집계 
# --------------------------------------------------
# 빈도분석
age_freq <- table(ctg$age)
age_freq

age_f_freq <- table(ctg$age_f)
age_f_freq

# 빈도분석 결과를 데이터프레임형태로 변환
age_df <- as.data.frame(age_f_freq)
age_df
View(age_df)

# 비율분석
age_prop <- prop.table(age_freq)
age_prop

age_f_prop <- prop.table(age_f_freq)
age_f_prop

# 비율분석 결과를 데이터프레임형태로 변환
age_prop_df <- as.data.frame(age_f_prop)
age_prop_df
View(age_prop_df)

# 비율을 기준으로 데이터프레임 내림차순 정렬
library(dplyr)
arrange(age_prop_df, desc(Freq))

# 비율분석 소수자리정리
round(age_prop, 3)
round(age_f_prop, 3)

# 백분율분석 
age_pect <- round(age_prop, 3) * 100
age_pect
age_f_pect <- round(age_f_prop, 3) * 100
age_f_pect

# 백분율분석 결과를 데이터프레임 형태로 변환
age_pect_df <- as.data.frame(age_f_pect)
age_pect_df
View(age_pect_df)

# 백분율을 기준으로 데이터프레임 내림차순 정렬
library(dplyr)
arrange(age_pect_df, desc(Freq))


# 연령대변수 시각화
# --------------------------------------------------
# barplot() 함수를 이용한 범주형 데이터 그래프

par(mfrow=c(2, 2)) # 멀티 캔버스 프레임 구성

barplot(age_f_freq,
        main="연령대 인원수 분포비교",
        xlab="연령대", ylab="명수")

barplot(age_f_freq,
        main="연령대 인원수 분포비교",
        xlab="연령대", ylab="명수", las = 1,
        horiz=TRUE)

barplot(age_f_prop,
        main="연령대 비율 분포비교",
        xlab="연령대", ylab="명수",
        density=c(20, 30),
        legend=rownames(age_f_freq))

barplot(age_f_prop,
        main="연령대 비율 분포비교",
        xlab="연령대", ylab="명수", las = 1,
        horiz=TRUE, col=c("lightblue", "pink"), 
        beside=TRUE, legend=rownames(age_f_freq))

par(mfrow=c(1, 1)) # 멀티 캔버스 프레임 리셋



### End of Source ####################################################

