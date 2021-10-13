######################################################################
# [03차시: R자료형과 데이터셋로딩] 03. 데이터셋 로딩하기
######################################################################

# --------------------------------------------------
# 기본패키지(base package)에서 샘플데이터 로딩
# --------------------------------------------------

# 현재 메모리에 로딩된 패키지 목록 확인
search()

# R내장 데이터셋 패키지에 대한 도움말 조회
help(package = datasets)
library(help = datasets)

# datasets 패키지에 있는 샘플데이터셋 내용을 콘솔상에 조회출력
iris # 붓꽃(iris) 품종별 생장특성 데이터
airquality # 뉴욕시 대기질측정 데이터
mtcars # 자동차 연비성능평가 데이터

# datasets 패키지에 있는 샘플데이터셋 메모리로 로딩
data(iris)
data(airquality)
data(mtcars)

# airquality 샘플데이터셋에 대한 도움말 확인
help(airquality)


# --------------------------------------------------
# 기여패키지(contributed package)에서 샘플데이터 로딩
# --------------------------------------------------

# 필요한 패키지 다운로드 및 설치
install.packages('ggplot2')

# 패키지기능 메모리로 로딩
library(ggplot2)

# 패키지 도움말 조회
help(package = ggplot2)
library(help = ggplot2)

# 패키지에 들어 있는 예제데이터셋 목록조회
data(package = 'ggplot2')

# ggplot2 패키지에 있는 mpg 예제데이터셋 로딩
data(mpg, package = 'ggplot2')
mpg

# mpg 샘플데이터셋에 대한 도움말 확인
help(mpg)


# --------------------------------------------------
# 기본 utils 패키지 read.table() 기본함수로 플레인텍스트(plain-text) 파일형식 로딩
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

# customer라는 표현이 포함된 파일목록 조회
list.files(pattern = 'customer')


# 로딩대상 플레인텍스트셋의 파일인코등(encoding) 형식 파악
#intall.packages('readr')
library(readr)

guess_encoding('customer.csv')
guess_encoding('customer2.csv')


# 일반적인 플레인텍스트파일 로딩방법
# --------------------------------------------------
# 플레인텍스트파일 기본로딩
cs <- read.table(file = 'customer.csv', 
                 header = TRUE, sep = ',')
cs
class(cs)
str(cs)

# 플레인텍스트파일 노이즈처리 옵션사용 로딩
cs <- read.table(file = 'customer.csv', 
                 header = TRUE, sep = ',',
                 fileEncoding = 'euc-kr',
                 stringsAsFactors = FALSE, 
                 strip.white = TRUE,
                 na.strings = c('.', '?', ' '))
cs
class(cs)
str(cs)


# 플레인텍스트파일에 부가사항이 있을 때 로딩방법
# --------------------------------------------------
# 플레인텍스트파일 부가사항처리 옵션사용 로딩
cs2 <- read.table(file = 'customer2.csv', 
                  header = FALSE, sep = '\t',
                  fileEncoding = 'euc-kr',
                  stringsAsFactors = FALSE, 
                  strip.white = TRUE, 
                  na.strings = c('.', '?', ' '),
                  skip = 3)
cs2
class(cs2) 
str(cs2)

# cs2 데이터프레임 변수컬럼명 확인과 변경
names(cs2)
names(cs2) <- c('name', 'gender', 'job', 'age', 
                'grade', 'survey', 'total', 'result')
cs2


# --------------------------------------------------
# 최신 readr패키지 read_delim() 로딩함수로 
# 플레인텍스트(plain-text) 파일형식 로딩
# --------------------------------------------------

# 필요 패키지 설치 및 로딩
# install.packages('readr')
library(readr)


# 일반적인 플레인텍스트파일 로딩방법
# --------------------------------------------------
cs_tb <- read_delim(file = 'customer.csv', col_names = TRUE, 
                    delim = ',', trim_ws = TRUE, na = c('.', '?'),  
                    locale = locale(encoding = 'EUC-KR')) 
cs_tb
class(cs_tb)
str(cs_tb)


# 플레인텍스트파일에 부가사항이 있을 때 로딩방법
# --------------------------------------------------
cs2_tb <- read_delim(file = 'customer2.csv', col_names = FALSE, 
                     delim = '\t', trim_ws = TRUE, na = c('.', '?'), 
                     skip = 3, locale = locale(encoding = 'EUC-KR')) 
cs2_tb
class(cs2_tb)
str(cs2_tb)


# cs2_tb 데이터프레임 변수컬럼명 확인과 변경
names(cs2_tb)
cs2_tb <- dplyr::rename(cs2_tb, 'name' = X1, 'gender' = X2, 
                        'job' = X3, 'age' = X4, 'grade' = X5, 
                        'survey' = X6, 'total' = X7, 'result' = X8)
names(cs2_tb)              


# --------------------------------------------------
# 최신 data.table패키지 fread() 로딩함수로 
# 플레인텍스트(plain-text) 파일형식 로딩
# --------------------------------------------------

# 필요 패키지 설치 및 로딩
# install.packages('data.table')
library(data.table)

# 일반적인 플레인텍스트파일 로딩방법
# --------------------------------------------------
cs_dt <- fread(input = 'customer.csv', header = TRUE, sep = ',', 
               stringsAsFactors = FALSE, 
               strip.white = TRUE, 
               na.strings = c('.', '?'))
cs_dt
class(cs_dt)
str(cs_dt)


# 플레인텍스트파일에 부가사항이 있을 때 로딩방법
# --------------------------------------------------
cs2_dt <- fread(input = 'customer2.csv', header = FALSE, sep = '\t', 
               stringsAsFactors = FALSE, 
               strip.white = TRUE, 
               na.strings = c('.', '?'),
               skip = 3)
cs2_dt
class(cs2_dt)
str(cs2_dt)


# cs2_dt 데이터프레임 변수컬럼명 확인과 변경
names(cs2_dt)
cs2_dt <- dplyr::rename(cs2_dt, 'name' = V1, 'gender' = V2, 'job' = V3, 
                        'age' = V4, 'grade' = V5, 'survey' = V6, 
                        'total' = V7, 'result' = V8)
names(cs2_dt)              


# --------------------------------------------------
# 최신 readxl패키지 read_excel() 로딩함수로 엑셀 파일형식 로딩
# --------------------------------------------------

# 필요 패키지 설치 및 로딩
# install.packages('readxl')
library(readxl)

# 엑셀파일에 포함된 시트목록 조회
excel_sheets('customer.xlsx')

# 일반적인 엑셀시트 로딩방법
# --------------------------------------------------
cs_xl <- read_excel(path = 'customer.xlsx', 
                    sheet = 'mycustomer',
                    col_names = TRUE, 
                    trim_ws = TRUE, 
                    na = c('.', '?')) 
cs_xl
class(cs_xl)
str(cs_xl)

# dlyr::glimpse() 함수를 이용한 데이터프레임 내부구조조회
# install.packages('dplyr')
library(dplyr)
glimpse(cs_xl)


# 엑셀시트에 부가사항이 있을 때 로딩방법
# --------------------------------------------------
cs2_xl <- read_excel(path = 'customer.xlsx', 
                     sheet = 'mycustomer2',
                     col_names = FALSE, 
                     trim_ws = TRUE, 
                     na = c('.', '?'),
                     range = 'B5:I10') 
cs2_xl
class(cs2_xl)
str(cs2_xl)


# cs2_xl 데이터프레임 변수컬럼명 확인과 변경
colnames(cs2_xl)
cs2_xl <- rename(cs2_xl, 'name' = 1, 'gender' = 2, 'job' = 3, 'age' = 4, 
                 'grade' = 5, 'survey' = 6, 'total' = 7, 'result' = 8)
str(cs2_xl)  


# --------------------------------------------------
# 최신 jsonlite패키지 fromJSON() 로딩함수로 JSON 파일형식 로딩
# --------------------------------------------------

# 필요 패키지 설치 및 로딩
# install.packages('jsonlite')
library(jsonlite)


# json 데이터셋 로딩방법
# --------------------------------------------------
cs_js <- fromJSON(txt = 'customer.json') 

cs_js
class(cs_js)
str(cs_js)


# --------------------------------------------------
# 인터넷상의 데이터셋을 온라인으로 로딩
# --------------------------------------------------

# UCI 머신러닝 저장소 사이트 접속
browseURL('http://archive.ics.uci.edu/ml')

# 다운로드 대상파일 링크 준비
url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data'
url
class(url)

# 기본패키지 read.table() 함수를 이용해 온라인상의 데이터셋파일 로딩
iris <- read.table(file = url, header = FALSE, sep = ',')
iris
class(iris)
str(iris)

# readr::read_delim() 함수를 이용해 온라인상의 데이터셋파일 로딩
iris_tb <- read_delim(file = url, col_names = FALSE, delim = ',')
iris_tb
class(iris_tb)
str(iris_tb)

# iris 데이터셋 변수컬럼 목록 확인과 변경
names(iris_tb)
names(iris_tb) <- c('sepal_length', 'sepal_width', 
                    'petal_length', 'petal_width', 
                    'type')
names(iris_tb)


### End of Source ####################################################


