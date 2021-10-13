######################################################################
# [07차시: 데이터 가공] 01. 변수 리코딩 하기
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

my
class(my)
str(my)


# --------------------------------------------------
# 범주형 변수에 대한 리코딩
# --------------------------------------------------


# 성별(gender)변수 기본탐색
# --------------------------------------------------
str(my$gender)
table(my$gender)
Hmisc::describe(my$gender)


# 성별(gender)변수 원본 수치형에서 다른 수치형으로 리코딩
# --------------------------------------------------
# 대괄호연산자 조건비교 이용
my$gender_n1[my$gender == 1] <- 5 
my$gender_n1[my$gender == 2] <- 10 

# base::ifelse() 함수이용
my$gender_n2 <- ifelse (my$gender == 1, 
                        yes = 5, no = 10)

# dplyr::recode() 함수이용
library(dplyr)
my$gender_n3 <- dplyr::recode(my$gender, '1' = 5 , '2' = 10)

# 리코딩결과 변수컬럼명으로 확인
head(my)

# 정규표현식으로 성별 관련변수들만 인덱싱
gd_idx <- grep(names(my), pattern = '^gender[:alpha:]{0,}')
gd_idx
gd_names <- grep(names(my), pattern = '^gender[:alpha:]{0,}', value = TRUE)
gd_names
head(my[gd_names])

# 리코딩 결과 변수특성으로 확인
# gender관련 변수들 빈도분석
sapply(my[gd_names], table) 
# gender관련 변수들 고유값 확인
sapply(my[gd_names], unique) 


# 성별(gender)변수 원본 수치형에서 문자형으로 리코딩
# --------------------------------------------------
# 대괄호연산자 조건비교 이용
my$gender_c1[my$gender == 1] <- '남성' 
my$gender_c1[my$gender == 2] <- '여성' 

# base::ifelse() 함수이용
my$gender_c2 <- ifelse (my$gender == 1, 
                        yes = '남성', no = '여성')

# dplyr::recode() 함수이용
library(dplyr)
my$gender_c3 <- dplyr::recode(my$gender, '1' = '남성' , '2' = '여성')

# 리코딩결과 변수컬럼명으로 확인
head(my)

# 정규표현식으로 성별 관련변수들만 인덱싱
gd_idx <- grep(names(my), pattern = '^gender_c[:alpha:]{0,}')
gd_idx
gd_names <- grep(names(my), pattern = '^gender_c[:alpha:]{0,}', value = TRUE)
gd_names
head(my[c('gender', gd_names)])

# 리코딩 결과 변수특성으로 확인
# gender관련 변수들 빈도분석
sapply(my[c('gender', gd_names)], table) 
# gender관련 변수들 고유값 확인
sapply(my[c('gender', gd_names)], unique) 


# 성별(gender)변수 문자형에서 논리형으로 리코딩
# --------------------------------------------------
# 대괄호연산자 조건비교 이용
my$gender_l1[my$gender_c1 == '남성'] <- TRUE 
my$gender_l1[my$gender_c1 == '여성'] <- FALSE 

# base::ifelse() 함수이용
my$gender_l2 <- ifelse (my$gender_c1 == '남성', yes = TRUE, no = FALSE)

# dplyr::recode() 함수이용
library(dplyr)
my$gender_l3 <- dplyr::recode(my$gender_c1, '남성' = TRUE , '여성' = FALSE)

# 리코딩결과 변수컬럼명으로 확인
head(my)

# 정규표현식으로 성별 관련변수들만 인덱싱
gd_idx <- grep(names(my), pattern = '^gender_l[:alpha:]{0,}')
gd_idx
gd_names <- grep(names(my), pattern = '^gender_l[:alpha:]{0,}', value = TRUE)
gd_names
head(my[c('gender_c1', gd_names)])

# 리코딩 결과 변수특성으로 확인
# gender관련 변수들 빈도분석
sapply(my[c('gender_c1', gd_names)], table) 
# gender관련 변수들 고유값 확인
sapply(my[c('gender_c1', gd_names)], unique) 


# --------------------------------------------------
# 연속형 변수에 대한 리코딩
# --------------------------------------------------

# --------------------------------------------------
# 데이터셋 로딩
# --------------------------------------------------

# utils::read.csv()함수로 로딩
my <- read.csv(file = 'tour.csv', 
               header = TRUE, sep = ',',
               stringsAsFactors = FALSE, 
               strip.white = TRUE,
               na.strings = c('.', '?', 'NA'))

my
class(my)
str(my)


# 추천의지(recom)변수 기본탐색
# --------------------------------------------------
str(my$recom)
table(my$recom)
Hmisc::describe(my$recom)


# 추천의지(recom)변수 원본 수치형에서 다른 수치형으로 리코딩
# --------------------------------------------------
# 대괄호연산자 조건비교 이용
my$recom_n1[my$recom == 1] <- 50 
my$recom_n1[my$recom == 2] <- 60 
my$recom_n1[my$recom == 3] <- 70 
my$recom_n1[my$recom == 4] <- 80 
my$recom_n1[my$recom == 5] <- 90 

# base::ifelse() 함수이용
my$recom_n2 <- ifelse (my$recom == 1, yes = 50, 
                       ifelse (my$recom == 2, yes = 60,
                               ifelse (my$recom == 3, yes = 70,
                                       ifelse (my$recom == 4, yes = 80, no = 90))))

# dplyr::recode() 함수이용
library(dplyr)
my$recom_n3 <- dplyr::recode(my$recom, 
                             '1' = 50 , '2' = 60 , '3' = 70 , '4' = 80 , '5' = 90)

# 리코딩결과 변수컬럼명으로 확인
head(my)

# 정규표현식으로 추천의지 관련변수들만 인덱싱
gd_idx <- grep(names(my), pattern = '^recom[:alpha:]{0,}')
gd_idx
gd_names <- grep(names(my), pattern = '^recom[:alpha:]{0,}', value = TRUE)
gd_names
head(my[gd_names])

# 추천의지 관련 리코딩 결과를 변수특성(빈도수와 고유치)로  확인
sapply(my[gd_names], table) 
sapply(my[gd_names], unique) 



# 추천의지(recom)변수 원본 수치형에서 문자형으로 리코딩
# --------------------------------------------------
# 대괄호연산자 조건비교 이용
my$recom_c1[my$recom == 1] <- '매우불만족' 
my$recom_c1[my$recom == 2] <- '불만족' 
my$recom_c1[my$recom == 3] <- '보통' 
my$recom_c1[my$recom == 4] <- '만족' 
my$recom_c1[my$recom == 5] <- '매우만족' 

# base::ifelse() 함수이용
my$recom_c2 <- ifelse (my$recom == 1, yes = '매우불만족', 
                       ifelse (my$recom == 2, yes = '불만족',
                               ifelse (my$recom == 3, yes = '보통',
                                       ifelse (my$recom == 4, yes = '만족', no = '매우만족'))))

# dplyr::recode() 함수이용
library(dplyr)
my$recom_c3 <- dplyr::recode(my$recom, 
                             '1' = '매우불만족' , '2' = '불만족' , 
                             '3' = '보통' , '4' = '만족' , '5' = '매우만족')

# 리코딩결과 변수컬럼명으로 확인
head(my)

# 정규표현식으로 추천의지 관련변수들만 인덱싱
gd_idx <- grep(names(my), pattern = '^recom_c[:alpha:]{0,}')
gd_idx
gd_names <- grep(names(my), pattern = '^recom_c[:alpha:]{0,}', value = TRUE)
gd_names
head(my[c('recom', gd_names)])

# 추천의지 관련 리코딩 결과를 변수특성(빈도수와 고유치)로  확인
sapply(my[c('recom', gd_names)], table) 
sapply(my[c('recom', gd_names)], unique) 



# 추천의지(recom)변수 원본 수치형을 리버스스케일링
# --------------------------------------------------
# 대괄호연산자 조건비교 이용
my$recom_r1[my$recom == 1] <- 5 
my$recom_r1[my$recom == 2] <- 4 
my$recom_r1[my$recom == 3] <- 3 
my$recom_r1[my$recom == 4] <- 2 
my$recom_r1[my$recom == 5] <- 1 

# base::ifelse() 함수이용
my$recom_r2 <- ifelse (my$recom == 1, yes = 5, 
                       ifelse (my$recom == 2, yes = 4,
                               ifelse (my$recom == 3, yes = 3,
                                       ifelse (my$recom == 4, yes = 2, no = 1))))

# dplyr::recode() 함수이용
library(dplyr)
my$recom_r3 <- dplyr::recode(my$recom, 
                             '1' = 5 , '2' = 4 , 
                             '3' = 3 , '4' = 2 , '5' = 1)

# 리코딩결과 변수컬럼명으로 확인
head(my)

# 정규표현식으로 추천의지 관련변수들만 인덱싱
gd_idx <- grep(names(my), pattern = '^recom_r[:alpha:]{0,}')
gd_idx
gd_names <- grep(names(my), pattern = '^recom_r[:alpha:]{0,}', value = TRUE)
gd_names
head(my[c('recom', gd_names)])

# 추천의지 관련 리코딩 결과를 변수특성(빈도수와 고유치)로  확인
sapply(my[c('recom', gd_names)], table) 
sapply(my[c('recom', gd_names)], unique)


### End of Source ####################################################

