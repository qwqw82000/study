######################################################################
# [12차시: 외래관광객 특성요인 상세분석] 01. 관광객특성요인에 따른 2차원 요약집계
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
library(janitor) ; library(epiDisplay)
tabyl(my$nat_c) %>% arrange(desc(n)) %>% View
tab1(my$nat_c, sort.group = "increasing", horiz = TRUE,
     cum.percent = TRUE, main = '거주국가 빈도분포')


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
# 거주국가에 따른 2차원 요약집계분석
# --------------------------------------------------

# 거주국가(nat)에 따른 체류기간(period) 요약집계 
# --------------------------------------------------
pd_nat_my <- aggregate(period ~ nat_c, my, mean)  %>% print
pd_nat_my2 <- aggregate(period ~ nat_c, my2, mean) %>% print
pd_nat_my3 <- aggregate(period ~ nat_c, my3, mean) %>% print
pd_nat_my3_trim <- aggregate(period ~ nat_c, my3, 
                             mean, trim = 0.05) %>% print

pd_nat_df <- data.frame(pd_nat_my, pd_nat_my2[2], pd_nat_my3[2], pd_nat_my3_trim[2]) %>% print
pd_nat_df %>% format(digits = 2) %>% arrange(desc(period.2)) %>% print

# 거주국가(nat)에 따른 체류기간(period) 요약집계 시각화
library(ggplot2)
p1 <- ggplot(data = pd_nat_df, 
             aes(x = reorder(nat_c, period.2), y = period.2)) + 
  geom_bar(stat="identity", fill="steelblue") + coord_flip() +
  ggtitle("거주국가(nat)에 따른 체류기간(period) 비교")
p1


# 거주국가(nat)에 따른 지출경비(expense) 요약집계 
# --------------------------------------------------
ep_nat_my <- aggregate(expense ~ nat_c, my, mean)  %>% print
ep_nat_my2 <- aggregate(expense ~ nat_c, my2, mean) %>% print
ep_nat_my3 <- aggregate(expense ~ nat_c, my3, mean) %>% print
ep_nat_my3_trim <- aggregate(expense ~ nat_c, my3, 
                             mean, trim = 0.05) %>% print

ep_nat_df <- data.frame(ep_nat_my, ep_nat_my2[2], ep_nat_my3[2], ep_nat_my3_trim[2]) %>% print
ep_nat_df %>% format(digits = 2) %>% arrange(desc(expense.2)) %>% print


# 거주국가(nat)에 따른 지출경비(expense) 요약집계 시각화
p2 <- ggplot(data = ep_nat_df, 
             aes(x = reorder(nat_c, expense.2), y = expense.2)) + 
  geom_bar(stat="identity", fill = "#FF6666") + coord_flip() +
  ggtitle("거주국가(nat)에 따른 지출경비(expense) 비교")
p2


# 거주국가(nat)에 따른 전반적만족도(overal) 요약집계 
# --------------------------------------------------
ov_nat_my <- aggregate(overal ~ nat_c, my, mean)  %>% print
ov_nat_my2 <- aggregate(overal ~ nat_c, my2, mean) %>% print
ov_nat_my3 <- aggregate(overal ~ nat_c, my3, mean) %>% print
ov_nat_my3_trim <- aggregate(overal ~ nat_c, my3, 
                             mean, trim = 0.05) %>% print

ov_nat_df <- data.frame(ov_nat_my, ov_nat_my2[2], ov_nat_my3[2], ov_nat_my3_trim[2]) %>% print
ov_nat_df %>% format(digits = 3) %>% arrange(desc(overal.2)) %>% print


# 거주국가(nat)에 따른 전반적만족도(overal) 요약집계 시각화
p3 <- ggplot(data = ov_nat_df, 
             aes(x = reorder(nat_c, overal.2), y = overal.2)) + 
  geom_bar(stat="identity", fill = "orange") + coord_flip() +
  ggtitle("거주국가(nat)에 따른 전반적만족도(overal) 비교")
p3


# 거주국가(nat)에 따른 요약집계 종합 
# --------------------------------------------------
nat_effect <- data.frame(pd_nat_df[c(1, 4)], 
                         ep_nat_df[4], 
                         ov_nat_df[4]) %>% 
  format(digit = 2) %>% print 

names(nat_effect) <- c('nat', 'period', 'expense', 'overal')

nat_effect %>% arrange(desc(period), desc(expense)) %>% print                   


# 거주국가(nat)에 따른 요약집계 종합 시각화
# --------------------------------------------------
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)


### End of Source ####################################################

