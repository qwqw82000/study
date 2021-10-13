######################################################################
# [04차시: 분석과제 모델링과 데이터 기본탐색] 02. 데이터 기본탐색
######################################################################

# --------------------------------------------------
# 데이터셋 로딩
# --------------------------------------------------

# 작업경로와 데이터셋 준비
# --------------------------------------------------
# 현재작업경로(working directory) 확인
getwd()
setwd('c:/Rtest')
getwd()
 
# 현재작업경로내 폴더와 파일 목록 조회
dir()
list.files()


# 플레인텍스트파일 로딩
# --------------------------------------------------

# utils::read.csv()함수로 플레인텍스트파일  로딩
my <- read.csv(file = 'tour.csv', 
               header = TRUE, sep = ',',
               stringsAsFactors = TRUE, 
               strip.white = TRUE,
               na.strings = c('.', '?', 'NA'))
my
class(my)
str(my)

# readr::read_delim()함수로 플레인텍스트파일 로딩
# install.packages('readr')
library(readr)

my_tb <- read_delim(file = 'tour.csv', 
                    col_names = TRUE, 
                    delim = ',', 
                    trim_ws = TRUE, 
                    na = c('.', '?', 'NA')) 
my_tb
class(my_tb)
str(my_tb)

# data.table::fread()함수로 플레인텍스트파일 로딩
# install.packages('data.table')
library(data.table)

my_dt <- fread(input = 'tour.csv', 
               header = TRUE, sep = ',', 
               stringsAsFactors = FALSE, 
               strip.white = TRUE, 
               na.strings = c('.', '?', 'NA'))
my_dt
class(my_dt)
str(my_dt)


# --------------------------------------------------
# 데이터셋 간단탐색
# --------------------------------------------------

# 데이터셋 내용파악
# --------------------------------------------------
# 데이터셋 간단조회
head(my)
tail(my)
# - 전체 데이터셋 내용을 앞에서부터, 뒤에서부터 6개 레코드씩 간단하게 조회하여
#   어떠한 변수컬럼이 있는지, 어떠한 raw데이터로 구성이 되어 있는지 살펴봄

head(my, 10)
tail(my, 15)

# install.packages('psych')
library(psych)

headTail(my)
headTail(my, 10, 5)

# 데이터셋 전체내용조회
my
(my)
print(my)
View(my)

# 티블(tibble)객체로 로딩한 전체내용조회
my_tb

# 데이트테이블(data.table)객체로 로딩한 전체내용조회
my_dt


# 데이터셋 기본구조 파악
# --------------------------------------------------
# 데이터셋 차원(행과 열) 규모 파악
dim(my)

my_dim <- dim(my)
my_dim
my_dim[1]
my_dim[2]

sprintf('데이터셋의 규모 => 레코드갯수: %d개, 변수컬럼갯수: %d개', my_dim[1], my_dim[2])


# 데이터셋 행과 열 이름파악
# 행이름 파악
rownames(my)
row.names(my)

# 열이름 파악
colnames(my)
names(my)


# 데이터셋 내부구조파악
# --------------------------------------------------
# utils::str() 함수이용
str(my)
# - RStudio 4개화면영역 중 Environment 영역을 보면, my라는 데이터프레임 객체의 내부구조정보를 직접 확인해 볼 수 있음

# tibble::glimpse() 함수이용 데이터셋 내부구조파악
# intall.packages('dplyr')
library(dplyr)
glimpse(my)


# --------------------------------------------------
# 데이터셋 기본요약
# --------------------------------------------------

# 데이터셋 기본 기술통계 
summary(my)

# 기술통계 패키지이용 분석
# install.packages(c('psych', 'Hmisc', 'skimr'))

library(psych)
psych::describe(my)

library(Hmisc)
Hmisc::describe(my)

library(skimr)
skimr::skim(my)

# 메모리상에 로딩된 패키지 라이브러리 목록 조회
search()
# - 로딩된 패키지들간 우선순위 파악


### End of Source ####################################################

