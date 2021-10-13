######################################################################
# [11차시: 외래관광객 실태조사 분석] 03. 관광객특성요인에 따른 관계분석
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

# 2개 변수간 관련성 분석: 범주와 범주간
# - 독립변수: 거주국가(nat)
# - 종속변수: 동반형태(member)
#             만족활동(bestact)

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
# install.packages('janitor')
library(janitor) 

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
tab1(my$member_c, sort.group = "decreasing", 
     cum.percent = TRUE,
     main = '동반형태 빈도분포')


# --------------------------------------------------
# 만족활동(bestact)변수 탐색
# --------------------------------------------------

# 기본구조와 기술통계
str(my$bestact)
table(my$bestact)
summary(my$bestact)

library(Hmisc)
Hmisc::describe(my$bestact)

# 변수리코딩
from <- c(1:23) 
to <- c('쇼핑', '식도락', '온천·스파', '휴양·휴식', '뷰티관광', 
        '의료관광', '유흥·오락', '카지노', '테마파크', '스포츠활동', 
        '레포츠활동', '직업적스포츠활동', '시찰(산업시설등)', 
        '연수·교육·연구', '미팅·회의·학술대회·박람회참가', 
        '업무수행', '고궁·역사유적지방문', '자연경관감상', 
        '공연·민속행사·축제참가관람', '박물관·전시관방문', 
        '시티투어버스이용', '전통문화체험', '기타')

my$bestact_c <- recodeVar(my$bestact, src = from, tgt = to)


# 리코딩한 변수 기술통계 분석
bestact_freq <- table(my$bestact_c) %>% 
  sort(decreasing = TRUE) %>% head(5) %>%  print

bestact_prop <- bestact_freq %>% 
  prop.table %>% sort(decreasing = TRUE) %>% 
  head(5) %>% print

bestact_df <- data.frame(freq = c(bestact_freq), 
                         pect = c(round(bestact_prop*100, 3))) %>% print


# 빈도와 비율분석 전용 패키지를 활용한 기술통계분석 
# janitor::tabyl() 함수이용
tabyl(my$bestact_c, sort = TRUE) %>% View

# epiDisplay::tab1() 함수이용
tab1(my$bestact_c, sort.group = "decreasing", 
     cum.percent = TRUE, main = '만족활동 빈도분포')


# --------------------------------------------------
# 거주국가에 따른 여러요인간 관계설정
# --------------------------------------------------

# 2개 변수간 관련성 분석: 범주와 범주간
# - 독립변수: 거주국가(nat)
# - 종속변수: 동반형태(member)
#             만족활동(bestact)


# 거주국가(nat)에 따른 동반형태(member) 간 교차분석
# --------------------------------------------------

# 거주국가에 따른 동반형태 간 교차빈도분석
nat_member <- table(my$nat_c, my$member_c, 
                    dnn = c('nat', 'member')) %>% 
  as.data.frame %>% arrange(desc(Freq)) %>% print

head(nat_member); tail(nat_member)

# 거주국가별 동반형태 빈도 조회
library(dplyr)
nat_member_freq <- nat_member %>% group_by(nat) %>% 
  arrange(nat, desc(Freq)) %>% print(n=100)

# 거주국가별 동반형태 빈도수를 백분율로 계산
nat_bestact_prop <- nat_member_freq %>%  mutate(Pect = (Freq / sum(Freq)) * 100) %>% 
  group_by(nat) %>% arrange(nat, desc(Freq)) %>% print(n=100) %>% View

# 거주국가별 동반형태  시각화
library(ggplot2); library(gridExtra); library(scales)

p1 <- ggplot(data = nat_member, 
             aes(x = nat, y = Freq, fill = member)) + 
  geom_bar(stat="identity", show.legend = FALSE) + coord_flip()


p2 <- ggplot(nat_member, aes(x = nat, y = Freq, fill = member)) + 
  geom_bar(position = "fill",stat = "identity") + coord_flip() +
  scale_y_continuous(labels = percent_format())

grid.arrange(p1, p2, ncol = 2)


# 거주국가(nat)에 따른 만족활동(bestact) 간 교차분석
# --------------------------------------------------

# 거주국가에 따른 만족활동 간 교차빈도분석
nat_bestact <- 
  table(my$nat_c, my$bestact_c, 
        dnn = c('nat', 'bestact')) %>% 
  as.data.frame %>% arrange(desc(Freq)) %>% 
  print

head(nat_bestact)
tail(nat_bestact)

# 거주국가별 만족활동 빈도가 높은 상위3개 조회
library(dplyr)
nat_bestact_top3 <- nat_bestact %>% group_by(nat) %>% top_n(3) %>% 
  arrange(nat, desc(Freq)) %>% print(n=100)

# 거주국가별 상위3개 만족활동빈도수를 백분율로 계산
nat_bestact_top3 <- nat_bestact %>%  mutate(Pect = (Freq / sum(Freq)) * 100) %>% 
  group_by(nat) %>% top_n(3) %>% arrange(nat, desc(Freq)) %>% print(n=100)

# 거주국가별 상위5개 만족활동 시각화
library(ggplot2); library(gridExtra); library(scales)

p1 <- ggplot(data = nat_bestact_top3, 
             aes(x = nat, y = Freq, fill = bestact)) + 
  geom_bar(stat="identity", show.legend = FALSE) + coord_flip()


p2 <- ggplot(nat_bestact_top3, aes(x = nat, y = Freq, fill = bestact)) + 
  geom_bar(position = "fill",stat = "identity") + coord_flip() +
  scale_y_continuous(labels = percent_format())

grid.arrange(p1, p2, ncol = 2)




# magrittr::파이프연산자%>%, dplyr::데이터가공함수 이용 요약집계 
#install.packages('magrittr')
library(magrittr)
library(dplyr)

my %>% filter(!is.na(age)) %>% 
  group_by(age) %>% 
  dplyr::summarize(Avg = mean(expense), SD = sd(expense))

my %>% filter(!is.na(age)) %>% 
  group_by(age) %>% 
  dplyr::summarize(Avg = mean(expense), SD = sd(expense)) %>% 
  arrange(desc(Avg)) # 평균지출금액 기준으로 내림차순 정렬


# 3개 변수간 관련성 분석: 범주 + 범주 --> 연속
# - 독립변수: 거주국가(nat) + 동반형태(member)
# - 종속변수: 체류기간(period)
#             지출경비(expense)
#             전반적만족도(overal)


# 3개 변수간 관련성 분석: 범주 + 범주 --> 범주
# - 독립변수: 거주국가(nat) + 성별(gender)
# - 종속변수: 동반형태(meber)
#             만족활동(bestact)


# 국기준 2차원 분석

table(my$nat, my$member)
table(my$nat, my$bestact)
 
# 국가 + 멤버 => 다른 변수 3차원 분석
aggregate(period ~ nat + member, my, mean)
aggregate(expense ~ nat + member, my, mean)
aggregate(overal ~ nat + member, my, mean)

# 국가 + 성별 => 다른변수 3차원분ㅅ억
table(my$gender, my$nat, my$bestact)
table(my$gender, my$nat, my$member)


 

# 2개 변수간 관련성 분석
# - 독립변수: 거주국가(nat)
# - 종속변수: 체류기간(period)
#             지출경비(expense)
#             전반적만족도(overal)
#             동반형태(member)
#             만족활동(bestact)


# 박스플롯을 이용한 지출경비(expense) 변수 이상점 검출 
# --------------------------------------------------

# 연속형 변수중 지출경비(expense) 관련 변수 기술통계파악 
Hmisc::describe(my$expense)
# - 1만달러 이상은 극단치로 간주해 데이터에서 제외하는 것으로 전처리 방향성파악

cnt2 <- subset(cnt, expense <= 10000)

psych::describe(cnt2$expense)
Hmisc::describe(cnt2$expense)

# 박스플롯팅
bp <- boxplot(cnt2$expense)

# 박스플롯 내부구조 파악
str(bp)

# 박스플롯 내부구조 중 분위수(Quantile) 파악
bp$stats

# [1,]    0.1013 -> 하한선(lower fence)
# [2,]  595.9350 -> Q1: 1분위수(25%)
# [3,] 1000.0000 -> Q2: 2분위수(50%), 중앙값
# [4,] 1725.3325 -> Q3: 3분위수(75%)
# [5,] 3419.2500 -> 상한선(upper fence)

bp$stats[5] # 상한선값

# 박스플롯 내부구조 중 이상치항목 탐색
bp$out
summary(bp$out)
Hmisc::describe(bp$out)
# - 상한선(upper fence) 값보다 큰 값은 절단(trimming)하거나
#   또는 상한선값으로 조정(winsorizing)하는 방향으로 전처리 필요


# --------------------------------------------------
# 연속형 변수 이상치 처리
# --------------------------------------------------

# 지출경비(expense) 변수 이상치 처리 
# --------------------------------------------------
# 상한선(upper fence) 값보다 큰 값을 절단화(trimming)
library(dplyr)
cnt3 <- cnt %>% filter(expense <= bp$stats[5])

psych::describe(cnt3$expense)
Hmisc::describe(cnt3$expense)

# 상한선(upper fence) 값보다 큰 값을 상한선값으로 조정화(winsorizing)

cnt$expense_ws <- ifelse (cnt$expense > bp$stats[5], 
                          yes = bp$stats[5], 
                          no = cnt$expense)

psych::describe(cnt$expense_ws)
Hmisc::describe(cnt$expense_ws)


# 체류기간(period)변수 탐색
# --------------------------------------------------
psych::describe(my$period)
bp_period <- boxplot(my$period)
bp_period$out
bp_period$stats

bp_period$stats[5]

# 지출경비(expense) 변수 이상치 처리 
# --------------------------------------------------
# 상한선(upper fence) 값보다 큰 값을 절단화(trimming)
library(dplyr)
cnt3 <- cnt %>% filter(expense <= bp$stats[5])

psych::describe(cnt3$expense)
Hmisc::describe(cnt3$expense)

# 상한선(upper fence) 값보다 큰 값을 상한선값으로 조정화(winsorizing)

cnt$expense_ws <- ifelse (cnt$expense > bp$stats[5], 
                          yes = bp$stats[5], 
                          no = cnt$expense)

psych::describe(cnt$expense_ws)
Hmisc::describe(cnt$expense_ws)







# 변수리코딩
library(doBy)
my$tourtype_c <- recodeVar(my$tourtype,
                           src = c(1, 2, 3), 
                           tgt = c('개별여행', '단체여행', '에어텔여행'))

# 리코딩한 변수 기술통계 분석
tourtype_freq <- table(my$tourtype_c) %>% 
  sort(decreasing = TRUE) %>% print

tourtype_prop <- tourtype_freq %>% 
  prop.table %>% sort(decreasing = TRUE) %>% print

tourtype_df <- data.frame(freq = c(tourtype_freq), 
                          pect = c(round(tourtype_prop*100, 3))) %>% print

# 빈도와 비율분석 전용 패키지를 활용한 기술통계분석 
#install.packages('janitor')
library(janitor)
tabyl(my$tourtype_c, sort = TRUE)

#install.packages('epiDisplay')
library(epiDisplay)

par(mfrow = c(1, 2))

tab1(my$tourtype_c, sort.group = "decreasing", 
     cum.percent = TRUE,
     main = '여행형태 빈도분포')

tab1(my$tourtype_c, sort.group = "decreasing", 
     cum.percent = TRUE, bar.values = 'percent', 
     main = '여행형태 비율분포')

par(mfrow = c(1, 1))


# 만족활동(bestact) 변수
# --------------------------------------------------
# 기본구조와 기술통계
str(my$bestact)
table(my$bestact)
summary(my$bestact)
library(Hmisc)
Hmisc::describe(my$bestact)


# 변수리코딩
from <- c(1:23) 
to <- c('쇼핑', '식도락', '온천·스파', '휴양·휴식', '뷰티관광', 
        '의료관광', '유흥·오락', '카지노', '테마파크', '스포츠활동', 
        '레포츠활동', '직업적스포츠활동', '시찰(산업시설등)', 
        '연수·교육·연구', '미팅·회의·학술대회·박람회참가', 
        '업무수행', '고궁·역사유적지방문', '자연경관감상', 
        '공연·민속행사·축제참가관람', '박물관·전시관방문', 
        '시티투어버스이용', '전통문화체험', '기타')

my$bestact_c <- recodeVar(my$bestact, src = from, tgt = to)


# 리코딩한 변수 기술통계 분석
bestact_freq <- table(my$bestact_c) %>% 
  sort(decreasing = TRUE) %>% head(5) %>%  print

bestact_prop <- bestact_freq %>% 
  prop.table %>% sort(decreasing = TRUE) %>% 
  head(5) %>% print

bestact_df <- data.frame(freq = c(bestact_freq), 
                         pect = c(round(bestact_prop*100, 3))) %>% print

# 빈도와 비율분석 전용 패키지를 활용한 기술통계분석 
tabyl(my$bestact_c) %>% arrange(desc(n)) %>% View
tab1(my$bestact_c, sort.group = "decreasing", 
     cum.percent = TRUE, main = '만족활동 빈도분포') 


# 여행형태에 따른 만족활동 간 교차분석
# --------------------------------------------------

# 여행형태에 따른 만족활동 간 교차빈도분석
tt_ba <- table(my$tourtype_c, my$bestact_c, dnn = c('tourtype', 'bestact')) %>% 
  as.data.frame %>% arrange(desc(Freq)) %>% print
head(tt_ba); tail(tt_ba)

# 여행형태별 만족활동 빈도가 높은 상위5개 조회
library(dplyr)
tt_ba_top5 <- tt_ba %>% group_by(tourtype) %>% top_n(5) %>% 
  arrange(tourtype, desc(Freq)) %>% print

# 여행형태별 상위5개 만족활동빈도수를 백분율로 계산
tt_ba_top5 <- tt_ba %>%  mutate(Pect = (Freq / sum(Freq)) * 100) %>% 
  group_by(tourtype) %>% top_n(5) %>% arrange(tourtype, desc(Freq)) %>% print

# 여행형태별 상위5개 만족활동 시각화
library(ggplot2); library(gridExtra); library(scales)

p1 <- ggplot(data = tt_ba_top5, 
             aes(x = tourtype, y = Freq, fill = bestact)) + 
  geom_bar(stat="identity", show.legend = FALSE)

p2 <- ggplot(data = tt_ba_top5, 
             aes(x = tourtype, y = Freq, fill = bestact)) + 
  geom_bar(stat="identity", position ="dodge", show.legend = FALSE)

p3 <- ggplot(tt_ba_top5, aes(x = tourtype, y = Freq, fill = bestact)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format())

grid.arrange(p1, p2, p3, ncol = 3)


### End of Source ####################################################

