######################################################################
# [05차시: 개별변수 요약과 집계] 02. 연속형 변수 특성요약과 시각화
######################################################################

# --------------------------------------------------
# 데이터셋 로딩
# --------------------------------------------------
 
# 플레인텍스트파일 로딩
# --------------------------------------------------
# utils::read.csv()함수로 플레인텍스트파일  로딩
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
# 연속형 변수컬럼 별도 데이터셋으로 추출
# --------------------------------------------------

# 전체 변수컬럼명 파악
all_names <- names(my)
all_names

# 범주형 변수컬럼명 파악
ctg_names <- c('month', 'gender', 'edu', 'job', 
               'age', 'nat', 'other', 'object', 
               'member', 'accom', 'activity')

# 연속형 변수컬럼명 도출
cnt_names <- setdiff(all_names, ctg_names)
cnt_names

# 범주형 변수컬럼 데이터셋 추출
cnt <- my[cnt_names]
cnt <- cnt[-1] # 첫번째 변수컬럼인 id변수 제외


# 연속형 데이터셋 특성파악
# --------------------------------------------------
# 간단 조회
head(cnt, 3)

# 내부구조 조회
str(cnt)

# 기본 기술통계량 파악
summary(cnt)

#install.packages('skimr')
library(skimr) 

skim(cnt)


# --------------------------------------------------
# 등간형 변수컬럼 특성파악
# -------------------------------------------------

# 전반적만족도변수 기본특성 파악
# --------------------------------------------------
# 1: 매우불만족, 2: 불만족, 3: 보통, 4: 만족, 5: 매우만족

# 전반적만족도 전체내용
cnt$overal

# 전반적만족도 일부내용
head(cnt$overal, 10)
tail(cnt$overal, 15)
cnt$overal[10:20]

# 전반적만족도 구조파악
length(cnt$overal)
NROW(cnt$overal)
str(cnt$overal)

# 전반적만족도 기술통계량
summary(cnt$overal)

library(psych)
psych::describe(cnt$overal)

library(Hmisc)
Hmisc::describe(cnt$overal)

library(skimr)
skim(cnt$overal)


# 전반적만족도 변수 요약집계: 중심성(central tendency) 
# --------------------------------------------------

# 산술평균(arithmetic average)
mean(cnt$overal)
mean(cnt$overal, na.rm = TRUE)
mean(cnt$overal, na.rm = TRUE, trim = 0.3)

# 중앙값(median)
median(cnt$overal)

# 최빈값(mode)
overal_freq <- table(cnt$overal)
overal_freq
names(overal_freq)
names(which.max(overal_freq))

# 전반적만족도변수를 팩터형으로 변환
# 만족도수치에 레이블을 붙여 최빈값 구하기
cnt$overal_f <- factor(cnt$overal, levels = c(1, 2, 3, 4, 5),
                       labels = c('매우불만족', '불만족', '보통', '만족', '매우만족'))
head(cnt)

overal_f_freq <- table(cnt$overal_f)
overal_f_freq
names(overal_f_freq)
names(which.max(overal_f_freq))


# 전반적만족도 변수 요약집계: 변동성(dispersion) 
# --------------------------------------------------

# 분산(variance)
var(cnt$overal)
var(cnt$overal, na.rm = TRUE)

# 표준편차(standard deviation)
sd(cnt$overal)
sd(cnt$overal, na.rm = TRUE)

# 범위(range)
range(cnt$overal)
range(cnt$overal, na.rm = TRUE)

# 최대(maximum)/최소(minimum)
max(cnt$overal)
min(cnt$overal)


# 전반적만족도 변수 요약집계: 정규성(normality) 
# --------------------------------------------------

# 왜도와 첨도 계산용 패키지 설치와 로딩
# install.packages("fBasics")
library(fBasics)

# 왜도(skewness)
skewness(cnt$overal)
skewness(cnt$overal, na.rm = TRUE)

# 첨도(kurtosis)
kurtosis(cnt$overal)
kurtosis(cnt$overal, na.rm = TRUE)


# 전반적만족도 변수 시각화
# --------------------------------------------------
# 간단한 플로팅
plot(cnt$overal, type = "p", 
     pch = 21, bg = "blue")

# abline()을 이용한 가이드라인(안내선)을 그리기
abline(h = seq(from = 1, 
               to = 5, 
               by = 1), 
       col = "gray", lty = 2)
abline(v = seq(from = 1000, 
               to = 14000, 
               by = 1000), 
       col = "gray", lty = 2)


# 히스토그램과 확률밀도곡선
par(mfrow=c(2, 2))

hist(cnt$overal, main="hist(), Frequency 옵션")
hist(cnt$overal, probability=TRUE,
     main="hist(), Probabilty 옵션")
plot(density(cnt$overal), main="density() 확률밀도 옵션")

hist(cnt$overal, probability=TRUE,
     main="hist() 히스토그램과 density() 확률밀도함수 통합")
lines(density(cnt$overal))

par(mfrow=c(1, 1))

# 박스플롯
boxplot(cnt$overal,
        main="박스플롯",
        ylab="전반적만족도")


# --------------------------------------------------
# 비율형 변수컬럼 특성파악
# -------------------------------------------------

# 지출경비변수 기본특성 파악
# --------------------------------------------------
# 달러($) 단위로 분석함

# 지출경비 전체내용
cnt$expense

# 지출경비 일부내용
head(cnt$expense, 5)
tail(cnt$expense, 5)
cnt$expense[10:15]

# 지출경비 구조파악
length(cnt$expense)
NROW(cnt$expense)
str(cnt$expense)

# 지출경비 기술통계량
summary(cnt$expense)

library(psych)
psych::describe(cnt$expense)

library(Hmisc)
Hmisc::describe(cnt$expense)

library(skimr)
skim(cnt$expense)


# 지출경비 변수 요약집계: 중심성(central tendency) 
# --------------------------------------------------

# 산술평균(arithmetic average)
mean(cnt$expense)
mean(cnt$expense, na.rm = TRUE)
mean(cnt$expense, na.rm = TRUE, trim = 0.3)

# 중앙값(median)
median(cnt$expense)

# 최빈값(mode)
expense_freq <- table(cnt$expense)
expense_freq
sort(expense_freq, decreasing = TRUE)

names(expense_freq)
names(which.max(expense_freq))


# 지출경비 변수 요약집계: 변동성(dispersion) 
# --------------------------------------------------

# 분산(variance)
var(cnt$expense)
var(cnt$expense, na.rm = TRUE)

# 표준편차(standard deviation)
sd(cnt$expense)
sd(cnt$expense, na.rm = TRUE)

# 범위(range)
range(cnt$expense)
range(cnt$expense, na.rm = TRUE)

# 최대(maximum)/최소(minimum)
max(cnt$expense)
min(cnt$expense)


# 지출경비 변수 요약집계: 정규성(normality) 
# --------------------------------------------------

# 왜도와 첨도 계산용 패키지 설치와 로딩
# install.packages("fBasics")
library(fBasics)

# 왜도(skewness)
skewness(cnt$expense)
skewness(cnt$expense, na.rm = TRUE)

# 첨도(kurtosis)
kurtosis(cnt$expense)
kurtosis(cnt$expense, na.rm = TRUE)


# 지출경비 변수 시각화
# --------------------------------------------------
# 간단한 플로팅
plot(cnt$expense, type = "p", 
     pch = 21, bg = "blue")

# abline()을 이용한 가이드라인(안내선)을 그리기
abline(h = seq(from = 1, 
               to = 13000000, 
               by = 1000000), 
       col = "gray", lty = 2)
abline(v = seq(from = 1000, 
               to = 14000, 
               by = 1000), 
       col = "gray", lty = 2)


# 히스토그램과 확률밀도곡선
par(mfrow=c(2, 2))

hist(cnt$expense, main="hist(), Frequency 옵션")
hist(cnt$expense, probability=TRUE,
     main="hist(), Probabilty 옵션")
plot(density(cnt$expense), main="density() 확률밀도 옵션")

hist(cnt$expense, probability=TRUE,
     main="hist() 히스토그램과 density() 확률밀도함수 통합")
lines(density(cnt$expense))

par(mfrow=c(1, 1))

# 박스플롯
boxplot(cnt$expense,
        main="박스플롯",
        ylab='지출경비')



### End of Source ####################################################

