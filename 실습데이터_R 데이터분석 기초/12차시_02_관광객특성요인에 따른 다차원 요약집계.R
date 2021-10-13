######################################################################
# [12차시: 외래관광객 특성요인 상세분석] 02. 관광객특성요인에 따른 다차원 요약집계
######################################################################


# --------------------------------------------------
# 데이터셋 로딩
# --------------------------------------------------

# utils::read.csv()함수로 로딩
my <- read.csv(file = 'ftour.csv', 
               header = TRUE, sep = ',',
               stringsAsFactors = FALSE, 
               strip.white = TRUE,
               na.strings = c('.', '?', 'NA'))


# 필요 패키지 설치 및 로딩
library(readr)

# 일반적인 플레인텍스트파일 로딩방법
# --------------------------------------------------
my <- read_delim(file = 'ftour.csv', col_names = TRUE, 
                 delim = ',', trim_ws = TRUE, 
                 na = c('.', '?', 'NA')) 
# 간단 데이터셋 파악
my
summary(my)
str(my)

# --------------------------------------------------
# 거주국가에 따른 여러요인간 관계설정
# --------------------------------------------------

# 2개 변수간 관련성 분석: 범주와 연속간
# - 독립변수: 거주국가(nat)
# - 종속변수: 체류기간(period)
#             지출경비(expense)
#             전반적만족도(overal)


# --------------------------------------------------
# 거주국가(nat)변수 탐색
# --------------------------------------------------

# 거주국가(nat) 변수
# --------------------------------------------------
# 기본구조와 기술통계cp
str(my$nat)
table(my$nat)
summary(my$nat)
library(Hmisc)
Hmisc::describe(my$nat)


# 거주국가(nat)변수 원본 수치형에서 문자형으로 리코딩
# --------------------------------------------------
# plyr::mapvalues() 함수이용
# install.packages('plyr')
library(plyr)

src <- c(1:19)
tgt <- c('일본', '중국', '홍콩', 
         '싱가포르', '대만', '태국', 
         '말레이시아', '호주', 
         '미국', '캐나다', '영국', 
         '독일', '프랑스', '러시아', 
         '중동', '인도', '필리핀', 
         '인도네시아', '베트남')

my$nat_c <- plyr::mapvalues(my$nat, 
                            from = src, to = tgt)


# 거주국가(nat)변수 문자형으로 리코딩결과 기술통계분석
# --------------------------------------------------
library(magrittr)
my$nat_c %>% table %>% print %>% 
  sort(decreasing = TRUE) %>% as.data.frame 

Hmisc::describe(my$nat_c)

# 빈도와 비율분석 전용 패키지를 활용한 기술통계분석 
library(janitor); library(epiDisplay)
tabyl(my$nat_c) %>% arrange(desc(n)) %>% View
tab1(my$nat_c, sort.group = "increasing", horiz = TRUE,
     cum.percent = TRUE, main = '거주국가 빈도분포')


# --------------------------------------------------
# 동반형태(member)변수 탐색
# --------------------------------------------------
# 기본구조와 기술통계
str(my$member)
table(my$member)
summary(my$member)
Hmisc::describe(my$member)

# 변수리코딩
library(doBy)
my$member_c <- recodeVar(my$member,
                         src = c(1, 2, 3, 4, 5), 
                         tgt = c('혼자', '가족·친지', 
                                 '친구·연인', '직장동료', '기타'))

# 빈도와 비율분석 전용 패키지를 활용한 기술통계분석
tabyl(my$member_c) %>% arrange(desc(n)) %>% View
tab1(my$member_c, sort.group = "decreasing", 
     cum.percent = TRUE,
     main = '동반형태 빈도분포')


# --------------------------------------------------
# 체류기간(period)변수 탐색
# --------------------------------------------------

# 연속형변수인 체류기간(period) 관련 변수 기술통계파악 
# --------------------------------------------------
psych::describe(my$period)
Hmisc::describe(my$period)
table(my$period)
# - 한달 보다 큰 체류일정은 극단치로 간주해 데이터에서 제외하는 것으로 전처리 방향성파악

my2 <- subset(my, period <= 31)

# 논리적판단에 따른 이상치 전처리 결과확인
psych::describe(my2$period)
Hmisc::describe(my2$period)


# 박스플롯을 이용한 체류기간(period) 분포파악 
# --------------------------------------------------
# 박스플롯팅
bp <- boxplot(my2$period, main = '체류기간(period) 분포')

# 박스플롯 내부구조 파악
str(bp)

# 박스플롯 내부구조 중 분위수(Quantile) 파악
bp$stats

# [1,] 1 -> 하한선(lower fence)
# [2,] 4 -> Q1: 1분위수(25%)
# [3,] 5 -> Q2: 2분위수(50%), 중앙값
# [4,] 7 -> Q3: 3분위수(75%)
# [5,] 11 -> 상한선(upper fence)

bp$stats[5] # 상한선값

# 박스플롯 내부구조 중 이상치항목 탐색
bp$out
summary(bp$out)
Hmisc::describe(bp$out)
# - 상한선(upper fence) 값보다 큰 값은 절단(trimming)하거나
#   또는 상한선값으로 조정(winsorizing)하는 방향으로 전처리 필요
# - 여기서는 정상치로 간주해서 요약집계분석의 결과변수로 사용하길 결정함  


# --------------------------------------------------
# 지출경비(expense)변수 탐색
# --------------------------------------------------

# 연속형변수인 지출경비(expense) 관련 변수 기술통계파악 
# --------------------------------------------------
# - 앞서 체류기간(period)이 31일 보다 큰 데이터를 제외한 한달 미만의 데이터셋 my2를 활용

psych::describe(my2$expense)
Hmisc::describe(my2$expense)
# - 1만달러 보다 큰 지출경비는 극단치로 간주해 데이터에서 제외하는 것으로 전처리 방향성파악

my3 <- subset(my2, expense <= 10000)

# 논리적판단에 따른 이상치 전처리 결과확인
psych::describe(my3$expense)
Hmisc::describe(my3$expense)


# 박스플롯을 이용한 지출경비(expense) 분포파악 
# --------------------------------------------------
# 박스플롯팅
bp <- boxplot(my3$expense, main = '지출경비(expense) 분포')

# 박스플롯 내부구조 파악
str(bp)

# 박스플롯 내부구조 중 분위수(Quantile) 파악
bp$stats

# [1,]    0.1013 -> 하한선(lower fence)
# [2,]  578.8966 -> Q1: 1분위수(25%)
# [3,]  965.0200 -> Q2: 2분위수(50%), 중앙값
# [4,] 160.0000  -> Q3: 3분위수(75%)
# [5,] 3125.5000 -> 상한선(upper fence)

bp$stats[5] # 상한선값

# 박스플롯 내부구조 중 이상치항목 탐색
bp$out
summary(bp$out)
Hmisc::describe(bp$out)
# - 상한선(upper fence) 값보다 큰 값은 절단(trimming)하거나
#   또는 상한선값으로 조정(winsorizing)하는 방향으로 전처리 필요
# - 여기서는 정상치로 간주해서 요약집계분석의 결과변수로 사용하길 결정함


# --------------------------------------------------
# 전반적만족도(overal)변수 탐색
# --------------------------------------------------

# 연속형변수인 전반적만족도(overal) 관련 변수 기술통계파악 
# --------------------------------------------------
# - 앞서 체류기간(period)이 31일 보다 큰 데이터를 제외한 한달 미만의 데이터셋 my2를 활용해
#   지출경비(expense)도 1만달러 보다 큰 데이터를 제외한 1만달러 이하의 데이터셋 my3를 활용

psych::describe(my3$overal)
Hmisc::describe(my3$overal)
# - 5점척도 범위의 조작적정의 내용대로 조사된 데이터로 간주해 이상치는 없는 것으로 파악함


# 박스플롯을 이용한 전반적만족도(overal) 분포파악 
# --------------------------------------------------
# 박스플롯팅
bp <- boxplot(my3$overal, main = '전반적만족도(overal) 분포')

# 박스플롯 내부구조 파악
str(bp)

# 박스플롯 내부구조 중 분위수(Quantile) 파악
bp$stats

bp$stats[5] # 상한선값

# 박스플롯 내부구조 중 이상치항목 탐색
bp$out
summary(bp$out)
Hmisc::describe(bp$out)
# - 상한선(upper fence) 값보다 큰 값은 절단(trimming)하거나
#   또는 상한선값으로 조정(winsorizing)하는 방향으로 전처리 필요
# - 여기서는 정상치로 간주해서 요약집계분석의 결과변수로 사용하길 결정함
 

# --------------------------------------------------
# 거주국가에 따른 3차원 요약집계분석
# --------------------------------------------------

# 거주국가(nat)와 동반형태(member)에 따른 체류기간(period) 다차원 요약집계 
# --------------------------------------------------
pd_mb_nat <- aggregate(period ~ member_c + nat_c, my3, mean) %>% print

head(pd_mb_nat); tail(pd_mb_nat)

pd_mb_nat %>% arrange(nat_c, desc(period)) %>% 
  summarize(nat_c, member_c, period) %>% 
  format(digit = 2) %>% View


# 거주국가(nat)와 동반형태(member)에 따른 체류기간(period) 다차원 시각화
library(ggplot2)

## facet_grid방식의 그룹화 및 세분화를 이용한 산점도 그리기
# 거주국가(nat)에 따라 동반형태(member)와 체류기간(period) 간 관계패턴의 차이여부 시각화
ggplot(data = pd_mb_nat, aes(x = member_c, y = period)) + 
  geom_point(size = 3) + 
  facet_grid(~ nat_c) + theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  labs(title = "거주국가에 따라 동반형태와 체류기간 간 분포차이",
       x = "동반형태(member)", y = "체류기간(period")
  

# 거주국가(nat)와 동반형태(member)에 따른 지출경비(expense) 다차원 요약집계 
# --------------------------------------------------
ep_mb_nat <- aggregate(expense ~ member_c + nat_c, my3, mean) %>% print
head(ep_mb_nat); tail(pd_mb_nat)

ep_mb_nat %>% arrange(nat_c, desc(expense)) %>% summarize(nat_c, member_c, expense) %>% 
  format(digit = 2) %>% View


# 거주국가(nat)와 동반형태(member)에 따른 체류기간(period) 다차원 시각화

## facet_grid방식의 그룹화 및 세분화를 이용한 산점도 그리기
# 거주국가(nat)에 따라 동반형태(member)와 지출경비(expense) 간 관계패턴의 차이여부 시각화
ggplot(data = ep_mb_nat, aes(x = member_c, y = expense)) + 
  geom_point(size = 3) + 
  facet_grid(~ nat_c) + theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  labs(title = "거주국가에 따라 동반형태와 지출경비 간 분포차이",
       x = "동반형태(member)", y = "지출경비(expense)")


### End of Source ####################################################

