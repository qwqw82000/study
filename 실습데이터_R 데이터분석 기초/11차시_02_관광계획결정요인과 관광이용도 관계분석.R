######################################################################
# [11차시: 외래관광객 실태조사 분석] 02. 관광계획결정요인과 관광이용도 관계분석
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
# 여행형태에 따른 만족활동 간의 관계분석
# --------------------------------------------------
# - 범주변수와 범주변수간 관계


# 여행형태(tourtype) 변수
# --------------------------------------------------
# 기본구조와 기술통계
str(my$tourtype)
table(my$tourtype)
summary(my$tourtype)
library(Hmisc)
Hmisc::describe(my$tourtype)


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
# install.packages('janitor')
library(janitor)
tabyl(my$tourtype_c, sort = TRUE)

# install.packages('epiDisplay')
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

