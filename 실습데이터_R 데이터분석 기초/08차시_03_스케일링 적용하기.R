######################################################################
# [08차시: 데이터 정제] 03. 스케일링 적용하기
######################################################################

 
# --------------------------------------------------
# 데이터셋 로딩
# --------------------------------------------------

# utils::read.csv()함수로 로딩
my <- read.csv(file = 'tour.csv', 
               header = TRUE, sep = ',',
               stringsAsFactors = FALSE, 
               strip.white = TRUE,
               na.strings = c('.', '?', 'NA'))

# 간단 데이터셋 파악
my
class(my)
str(my)


# --------------------------------------------------
# 연속형 변수셋 전반적 분포특성 파악
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
cnt_names <- setdiff(all_names, c(ctg_names, 'id'))
cnt_names

# 연속형 변수컬럼으로 구성된 서브셋 도출
cnt <- my[cnt_names]


# 연속형 데이터셋 특성파악
# --------------------------------------------------
# 간단 조회
head(cnt, 3)

# 내부구조 조회
str(cnt)

# 기본 기술통계량 파악
library(skimr)
skim(cnt)


# 박스플롯을 이용한 연속형 변수 분포특성 파악 
# --------------------------------------------------

# 연속형 변수 전체 박스플롯을 통한 분포현황 탐지
boxplot(cnt)

# 연속형 변수중 비용관련 변수 박스플롯
boxplot(cnt[c('expense', 'card', 'cash')])

# 연속형 변수중 만족도관련 변수 박스플롯
boxplot(cnt[c("overal", "bf_img", "af_img", "revisit", "recom")])

# 연속형 변수중 나머지 관광행태 관련변수 박스플롯
boxplot(cnt[c("decision", "count", "number", "period")])


# --------------------------------------------------
# 연속형 변수 중 동반자수(number) 탐색
# --------------------------------------------------

# 동반자수(number) 변수 기본특성 파악
# --------------------------------------------------
library(psych)
library(Hmisc)

psych::describe(cnt$number)
Hmisc::describe(cnt$number)

table(cnt$number)


# 동반자수(number) 변수 극단치 조정(winsorizing)하기
# --------------------------------------------------

# doBy::recodeVar() 함수이용
# install.packages("doBy")
library(doBy)

from <- list(1, 2, 3, 4, 5, 6, 7, 8, 9, c(10:301))
to <- list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)


cnt$number10 <- recodeVar(cnt$number, 
                          src = from, tgt = to)

psych::describe(cnt$number10)
Hmisc::describe(cnt$number10)
table(cnt$number10)


# 멀티캔버스를 통한 동반자수(number) 분포특성 시각화
# --------------------------------------------------

par(mfrow=c(2, 2))

# 박스플롯
boxplot(cnt$number,
        main="극단치 조정전 박스플롯",
        ylab="동반자수")

boxplot(cnt$number10,
        main="극단치 조정후 박스플롯",
        ylab="동반자수")

# 히스토그램과 확률밀도곡선
hist(cnt$number, 
     main="극단치 조정전 히스토그램& 확률밀도함수 통합")
lines(density(cnt$number))

hist(cnt$number10,
     main="극단치 조정후 히스토그램과 확률밀도함수 통합")
lines(density(cnt$number10))

par(mfrow=c(1, 1))


# --------------------------------------------------
# 연속형 변수 중 동반자수(number) 스케일링(scaling)
# --------------------------------------------------


# 정규화(normalization): 센터링옵션(centering) 
# --------------------------------------------------
# ==> -1 ~ +1 사이로 변환
# - ctr: centering

number10_ctr <- cnt$number10 %>% scale(center = TRUE)
summary(number10_ctr)
psych::describe(number10_ctr)

# 정규화(normalization): 레인징옵션(ranging) 
# --------------------------------------------------
# ==> 0 ~ 1 사이로 변환
# - ranging

number10_rng <- cnt$number10 %>% scale(center = FALSE)
summary(number10_rng)
psych::describe(number10_rng)

# 표준화(standarization): z-스코어(z-score) 
# --------------------------------------------------
# ==> 평균0, 표준편차가 1인 표준정규분포로 변환
# - snd: standard normal distribution

number10_snd <- cnt$number10 %>% scale(center = TRUE, scale = TRUE)
summary(number10_snd)
psych::describe(number10_snd)


# --------------------------------------------------
# 동반자수(number) 스케일링(scaling) 결과 시각화
# --------------------------------------------------

par(mfrow=c(2, 3))

boxplot(number10_ctr,
        main="정규화(normalization): 센터링옵션(centering)",
        ylab="동반자수")

boxplot(number10_rng,
        main="정규화(normalization): 레인징옵션(ranging)",
        ylab="동반자수")

boxplot(number10_snd,
        main="표준화(standardization): z-스코어(z-score)",
        ylab="동반자수")


hist(number10_ctr, probability = TRUE,
     main="정규화(normalization): 센터링옵션(centering)")
lines(density(number10_ctr))

hist(number10_ctr, probability = TRUE,
     main="정규화(normalization): 레인징옵션(ranging)")
lines(density(number10_ctr))

hist(number10_ctr, probability = TRUE,
     main="정규화(normalization): z-스코어(z-score)")
lines(density(number10_ctr))


par(mfrow=c(1, 1))


# --------------------------------------------------
# 연속형 변수 중 동반자수(number) 데이터변환(transformation)
# --------------------------------------------------

# 로그(log) 변환
# --------------------------------------------------

number10_lg <- log(cnt$number10)
summary(number10_lg)
psych::describe(number10_lg)

# 제곱근(square root) 변환
# --------------------------------------------------
number10_sq <- sqrt(cnt$number10)
summary(number10_sq)
psych::describe(number10_sq)


# --------------------------------------------------
# 동반자수(number) 데이터변환(transformation) 시각화
# --------------------------------------------------

# install.packages ("rcompanion")
library (rcompanion)

par(mfrow=c(1, 2))

plotNormalHistogram(number10_lg, main = '로그(log)변환')
plotNormalHistogram(number10_sq, main = '제곱근(square root)변환')

par(mfrow=c(1, 1))


### End of Source ####################################################



