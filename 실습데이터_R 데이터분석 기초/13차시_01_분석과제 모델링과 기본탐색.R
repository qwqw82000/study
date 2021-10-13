######################################################################
# [13차시: 건강정보 데이터 분석] 01. 분석과제 모델링과 기본탐색
######################################################################

 
# ------------------------------------------------------------------------
### 문제시나리오: 환자의 유방암 여부를 예측할 수 있는 분류규칙의 도출

## 환자의 유방암과 상관관계가 있는 9개 세포학적 특성
#  ==> 1: 양성가능성 매우약함 ~ 10: 매우 악성인 상황

# ID: 환자아이디
# **clumpThickness: 덩어리 두께 
# sizeUniformity" 셀크기 균일성
# shapeUniformity: 셀모양 균일성 
# **maginalAdhesion: 한계부착력
# singleEpithelialCellSize: 단일상피세포크기 
# **bareNuclei: 기저핵 
# **blandChromatin: 브랜드 염색질 
# **normalNucleoli: 정상 핵소체
# **mitosis: 유사 분열, 체세포 분열

# class: 클래스 ==> 2: 양성(benign), 4: 악성(malignant)


# --------------------------------------------------
# 분석에 필요한 패키지 일괄 설치 및 로딩
# --------------------------------------------------

# 필요한 패키지 목록 생성
pkg <- c('readr', 'dplyr', 'magrittr', 
         'skimr', 'Hmisc', 'psych', 
         'ggplot2', 'gridExtra', 'doBy', 'summarytools', 
         'PerformanceAnalytics', 'corrplot', 'naniar')

# 필요패키지 설지여부를 체크해 미설치패키지 목록을 저장
new_pkg <- pkg[!(pkg %in% rownames(installed.packages()))]

# 미설치 패키지 목록이 1개라도 있으면, 일괄 인스톨 실시
if (length(new_pkg)) install.packages(new_pkg, dependencies = TRUE)

# 필요패키지를 일괄 로딩실시
suppressMessages(sapply(pkg, require, character.only = TRUE))  
# - suppressMessages() 함수를 통해 패키지 로딩시 나타나는 복잡한 설명/진행상황 문구 출력억제


# --------------------------------------------------
# 데이터셋 준비
# --------------------------------------------------

## 예제 데이터 준비
# UC Irvine Machine Learning Repository 보유데이터 활용

# UCI 머신러닝 저장소 사이트 접속
browseURL('http://archive.ics.uci.edu/ml')


# 사이트 접속정보
loc_uci <- "http://archive.ics.uci.edu/ml/machine-learning-databases/" 

# 데이터 위치정보
loc_cancer  <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"

# 접속 URL 준비
url <- paste(loc_uci, loc_cancer, sep="")
url


# 기본패키지 read.table() 함수를 이용해 온라인상의 데이터셋파일 로딩
raw <- read.csv(file = url, 
                header = FALSE, sep = ',',
                stringsAsFactors = FALSE, 
                strip.white = TRUE,
                na.strings = c('.', '?', 'NA'))
head(raw)
str(raw)


# readr::read_delim() 함수를 이용해 온라인상의 데이터셋파일 로딩
library(readr)
my <- read_delim(file = url, 
                 col_names = FALSE, 
                 delim = ',', 
                 trim_ws = TRUE, 
                 na = c('.', '?', 'NA')) 
my
library(dplyr)
glimpse(my)


# --------------------------------------------------
# 데이터셋 기본정보 설정
# --------------------------------------------------

# 데이터셋 변수컬럼명 반영
names(my) <- c('id', 'thick', 'unisize', 'unishape', 'adhesion', 
               'cellsize', 'bareNuc', 'chromatin', 'normnuc', 
               'mitosis', 'class')

## 환자 아이디 변수컬럼 제외
my <- my[-1]


# --------------------------------------------------
# 데이터셋 기본구조 및 간단내용 조회
# --------------------------------------------------

# 정보설정내용 확인
str(my)

# 데이터셋 간단조회
head(my)
tail(my)
# - 전체 데이터셋 내용을 앞에서부터, 뒤에서부터 6개 레코드씩 간단하게 조회하여
#   어떠한 변수컬럼이 있는지, 어떠한 raw데이터로 구성이 되어 있는지 살펴봄

head(my, 10)
tail(my, 15)


# --------------------------------------------------
# 데이터셋 기술통계분석
# --------------------------------------------------

# 데이터셋 기본 기술통계 
summary(my)


# 기술통계 패키지이용 분석
psych::describe(my)
Hmisc::describe(my)
skimr::skim(my)

## 기술통계분석 새로운 패키지
library(summarytools)
summarytools::descr(my)
dfSummary(my)

# --------------------------------------------------
# 데이터셋 전체 산점도 매트릭스
# --------------------------------------------------

# 산점도 매트릭스: 전체변수
plot(my)


# 전체변수간 상관관계 매트릭스
# (일부 NA포함 변수는 출력안됨)
round(cor(my), 3)
round(cor(my, use = 'complete.obs'), 3)

# 데이터셋의 
# NA요소 제거 옵션설정 
# 상관관계 분석
temp <- cor(my, 
            use = 'complete.obs')
round(temp, 2)

# psych::pairs.panels() 함수이용
library(psych)
pairs.panels(my,  
             method = "spearman",
             hist.col = "green",
             density = TRUE,
             ellipses = TRUE)

# PerformanceAnalytics::chart.Correlation() 함수이용
library(PerformanceAnalytics)
chart.Correlation(my, histogram = T)

# corrplot::corrplot() 함수이용
library(corrplot)

corrplot(cor(my, use = 'complete.obs'))
corrplot.mixed(cor(my, use = 'complete.obs'))
corrplot(cor(my, use = 'complete.obs'), method = 'pie')


### End of Source ####################################################


