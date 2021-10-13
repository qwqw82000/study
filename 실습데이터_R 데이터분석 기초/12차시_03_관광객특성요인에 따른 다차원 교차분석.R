######################################################################
# [12차시: 외래관광객 특성요인 상세분석] 03. 관광객특성요인에 따른 다차원 교차분석
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
# 성별(gender)변수 탐색
# --------------------------------------------------

# 성별(gender) 변수
# --------------------------------------------------
# 기본구조와 기술통계
str(my$gender)
table(my$gender)
summary(my$gender)
library(Hmisc)
Hmisc::describe(my$gender)

# 성별(gender)변수 원본 수치형에서 문자형으로 리코딩
# --------------------------------------------------
# plyr::mapvalues() 함수이용
# install.packages('plyr')
library(plyr)

my$gender_c <- plyr::mapvalues(my$gender, 
                               from = c(1, 2), 
                               to = c('M', 'F'))


# 성별(gender)변수 문자형으로 리코딩결과 기술통계분석
# --------------------------------------------------
library(magrittr)
my$gender_c %>% table %>% print %>% 
  sort(decreasing = TRUE) %>% as.data.frame 

Hmisc::describe(my$gender_c)

# 빈도와 비율분석 전용 패키지를 활용한 기술통계분석 
tabyl(my$gender_c) %>% arrange(desc(n)) %>% print
tab1(my$gender_c, sort.group = "increasing",
     cum.percent = TRUE, main = '성별 빈도분포')


# --------------------------------------------------
# 동반형태(member)변수 탐색
# --------------------------------------------------
# 성별(gender) 변수
str(my$member)
table(my$member)
summary(my$member)
Hmisc::describe(my$member)

# 변수리코딩
# doBy::recodeVar() 함수이용
library(doBy)
my$member_c <- recodeVar(my$member,
                         src = c(1, 2, 3, 4, 5), 
                         tgt = c('혼자', '가족·친지', 
                                 '친구·연인', '직장동료', '기타'))
table(my$member_c)
# - 기존의 숫자보기항목이 문자로 리코딩 되면서, 뒤죽박죽 되어버림

# base::factor() 함수이용
my$member_f <- factor(my$member, levels = c(1, 2, 3, 4, 5), 
                      labels = c('혼자', '가족·친지', '친구·연인', '직장동료', '기타'))
table(my$member_f)
# - 기존의 숫자보기항목이 문자로 리코딩 되었지만, 팩터함수로 원래순서를 유지함

# 빈도와 비율분석 전용 패키지를 활용한 기술통계분석 
tab1(my$member_f, sort.group = "decreasing", 
     cum.percent = TRUE,
     main = '동반형태 빈도분포')


# --------------------------------------------------
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
  sort(decreasing = TRUE) %>%  print

bestact_prop <- bestact_freq %>% 
  prop.table %>% sort(decreasing = TRUE) %>% print

bestact_df <- data.frame(freq = c(bestact_freq), 
                         pect = c(round(bestact_prop*100, 3))) %>% print

# 빈도와 비율분석 전용 패키지를 활용한 기술통계분석 
tabyl(my$bestact_c) %>% arrange(desc(n)) %>% View
tab1(my$bestact_c, sort.group = "decreasing", 
     cum.percent = TRUE, main = '만족활동 빈도분포') 


# --------------------------------------------------
# 거주국가에 따른 다차원 교차분석
# --------------------------------------------------

# 거주국가(nat)와 성별(gender)에 따른 동반형태(member) 다차원 교차분석
# --------------------------------------------------
gd_mb_nat <- xtabs(~ gender_c + member_f + nat_c, data = my) %>% print
gd_mb_nat %>% ftable

nat_gd_mb <- xtabs(~ nat_c + gender_c + member_f, data = my) %>% print
nat_gd_mb %>% ftable


# 거주국가(nat)와 성별(gender)에 따른 동반형태(member) 다차원 시각화
library(ggplot2); library(gridExtra); library(scales)

ggplot(my, aes(gender_c, fill = member_f)) + 
  geom_bar() + facet_wrap( ~ nat_c)


# 거주국가(nat)와 성별(gender)에 따른 만족활동(bestact) 다차원 교차분석
# --------------------------------------------------
gd_ba_nat <- xtabs(~ gender_c + bestact_c + nat_c, data = my) %>% print
gd_ba_nat %>% ftable

nat_gd_ba <- xtabs(~ nat_c + gender_c + bestact_c, data = my) %>% print
nat_gd_ba %>% ftable


# 거주국가(nat)와 성별(gender)에 따른 만족활동(bestact) 다차원 시각화
library(ggplot2); library(gridExtra); library(scales)

ggplot(my, aes(gender_c, fill = bestact_c)) + 
  geom_bar() + facet_wrap( ~ nat_c)


### End of Source ####################################################


