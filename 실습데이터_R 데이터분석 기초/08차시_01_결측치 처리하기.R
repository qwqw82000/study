######################################################################
# [08차시: 데이터 정제] 01. 결측치 처리하기
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
# 결측값 현황파악
# --------------------------------------------------


# 결측값이 포함되어 있는지 확인하는 방법
# --------------------------------------------------
# NA관련 함수이용
anyNA(my) # 전체데이터셋 NA포함유무
is.na(my) # 각변수 요소별 NA유무

sapply(my, anyNA) # 각 변수별 NA포함유무


# 기술통계 함수이용
summary(my)
library(psych); library(Hmisc); library(skimr)
psych::describe(my)
Hmisc::describe(my)
skimr::skim(my)


# 결측값 규모파악
# --------------------------------------------------
library(magrittr)
my %>% is.na %>% sum
my %>% sapply(is.na) %>% colSums


# 결측치 패키지를 활용한 현황파악
# --------------------------------------------------
# install.packages('naniar')
library(naniar)

# NA갯수
n_miss(my) 

# 시그널갯수
n_complete(my)

# 변수별 NA현황
miss_var_summary(my) 

# 레코드별 NA현황
miss_case_summary(my) 

# NA패턴 시각화
vis_miss(my)


# --------------------------------------------------
# 결측값 제외/삭제(remove)
# --------------------------------------------------

# 특정변수에 대한 분석시 포함된 결측값을 제외하는 옵션사용
# --------------------------------------------------
# 범주형 변수 사용시 결측치 제외옵션 사용
table(my$age)
table(my$age, useNA = 'ifany')

table(my$age, my$accom)
table(my$age, my$accom, useNA = 'ifany')

# 연속형 변수 사용시 결측치 제외옵션 사용
mean(my$count)
mean(my$count, na.rm = TRUE)

cor(my$count, my$expense)
cor(my$count, my$expense, use = 'complete.obs')

# 범주형과 연속형간 요약집계시 결측치 제외옵션 사용
library(dplyr); library(magrittr)
# NA값을 포함한 상태 요약집계
my %>% select(age, count) %>%
  group_by(age) %>% dplyr::summarize(Avg = mean(count))

# NA값을 필터링으로 제외시킨 상태 요약집계
my %>% select(age, count) %>% filter(!is.na(age) & !is.na(count)) %>% 
  group_by(age) %>% dplyr::summarize(Avg = mean(count))


# 결측값 포함 변수/레코드를 데이터셋에서 제거하고 남은데이터로 분석
# --------------------------------------------------
# 원본데이터셋의 NA현황 재확인
summary(my)
dim(my)
anyNA(my)
my %>% is.na %>% sum

# NA포함 레코드/변수 일괄 삭제 전처리
# stats::na.omit()함수이용
my_omd <- na.omit(my)
anyNA(my_omd)
my_omd %>% 
  is.na %>% sum
dim(my_omd)

# stats::complete.cases()함수이용
my_cpd <- complete.cases(my)
anyNA(my_cpd)
my_cpd %>% is.na %>% sum


# NA포함 특정 레코드/변수를 부분적으로 삭제 전처리
# --------------------------------------------------
# 숙박시설(accom)에 포함된 NA값 현황과 삭제
summary(my$accom)
anyNA(my$accom)
my$accom %>% is.na %>% sum

my_na_accom <- na.omit(my$accom)
anyNA(my$na_accom)
my$na_accom %>% is.na %>% sum

# 1~100번 레코드에 포함된 NA값 현황과 삭제
summary(my[1:100, ])
anyNA(my[1:100, ])
my[1:100, ] %>% is.na %>% sum

my_na_100 <- complete.cases(my[1:100, ])
anyNA(my[1:100, ])
my$na_accom %>% is.na %>% sum


# --------------------------------------------------
# 결측값 단순대체(single imputation)
# --------------------------------------------------

# 특정변수의 NA값을 임의값으로 단순대체
# --------------------------------------------------

# 방문횟수(count)변수 NA현황
anyNA(my$count)
my$count %>% is.na %>% sum

# 방문횟수(count)변수의 NA를 대체할 대표치 산정
# 기본적으로 0으로 대체하거나 평균값, 중앙값, 최빈값을 활용함
count_avg <- my$count %>% mean(na.rm = TRUE) %>% print # 평균값
count_med <- my$count %>% median(na.rm =TRUE) %>% print # 중앙값
count_mode <- my$count %>% table %>% which.max %>% names %>% print # 최빈값

# 방문횟수(count)변수의 NA를 최빈값으로 대체
my$count[is.na(my$count)] <- count_mode

# 최빈값으로 NA를 대체한 방문횟수(count)변수의 NA값 재확인
anyNA(my$count)
my$count %>% is.na %>% sum


# 각 변수별 NA값을 각 변수별 중앙값으로 일괄 단순대체
# --------------------------------------------------
# my데이터셋에 포함된 NA 현황
anyNA(my)
is.na(my)
sapply(my, anyNA)

library(magrittr)
my %>% is.na %>% sum
my %>% sapply(is.na) %>% colSums

# my데이터셋에 포함된 NA를 각 변수별 중앙값으로 대체하는 함수정의 
# 각 변수별 중앙값 산정
sapply(my, median, na.rm = TRUE)

# 각 변수별 중앙값을 각 변수별 NA마다 대체입력
my <- sapply(my, function(x) 
  ifelse(is.na(x), yes = median(x, na.rm = TRUE), no = x)) %>% as.data.frame


# my데이터셋에 포함된 NA값 재확인
anyNA(my)
is.na(my)
sapply(my, anyNA)
library(magrittr)
my %>% is.na %>% sum
my %>% sapply(is.na) %>% colSums


### End of Source ####################################################

