######################################################################
# [13차시: 건강정보 데이터 분석] 02. 데이터 가공과 정제
######################################################################

 
# --------------------------------------------------
# 데이터셋 준비
# --------------------------------------------------


# 데이터셋 파일 온라인에서 로딩
# --------------------------------------------------
# 사이트 접속정보
loc_uci <- "http://archive.ics.uci.edu/ml/machine-learning-databases/" 

# 데이터 위치정보
loc_cancer  <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"

# 접속 URL 준비
url <- paste(loc_uci, loc_cancer, sep="")

# readr::read_delim()함수이용 온라인상의 데이터셋 로딩
library(readr)
my <- read_delim(file = url, col_names = FALSE, 
                 delim = ',', trim_ws = TRUE, 
                 na = c('.', '?', 'NA')) 
my

# 데이터셋 구조파악
library(dplyr)
glimpse(my)


# 데이터셋 파일 오프라인으로 저장
# --------------------------------------------------
# 데이터셋 변수컬럼명 반영
names(my) <- c('id', 'thick', 'unisize', 'unishape', 'adhesion', 
               'cellsize', 'barenuc', 'chromatin', 'normnuc', 
               'mitosis', 'class')

# 정보설정내용 확인
str(my)

# 메모리상의 사용중인 데이터셋 하드디스크로 저장
write.csv(my, file = 'cancer.csv', row.names = FALSE, quote = FALSE)


# 오프라인 데이터셋파일 메모리로 로딩
# --------------------------------------------------
# readr::read_delim()함수이용 플레인텍스트 파일 로딩
library(readr)
my <- read_delim(file = 'cancer.csv', col_names = TRUE, 
                 delim = ',', trim_ws = TRUE, 
                 na = c('.', '?', 'NA')) 
my

# 데이터셋 구조파악
library(dplyr)
glimpse(my)

## 환자 아이디 변수컬럼 제외
my <- my[-1]

# 데이터셋 구조파악
str(my)

# 데이터셋 간단조회
head(my); tail(my)

# 데이터셋 기본요약
summary(my)


# --------------------------------------------------
# 결측치 처리 
# --------------------------------------------------


# 데이터셋에 포함된 결측치 현황파악
# --------------------------------------------------
# NA관련 함수이용 결측치 포함유무 확인
anyNA(my)
is.na(my)
sapply(my, anyNA)

# 기술통계 함수이용 결측치 포함현황 파악
summary(my)
library(psych)
library(Hmisc) 
library(skimr)
psych::describe(my)
Hmisc::describe(my)
skimr::skim(my)

# 결측값 규모파악
library(magrittr)
my %>% is.na %>% sum
my %>% sapply(is.na) %>% colSums


# 결측치 패키지 이용 현황파악
# --------------------------------------------------
# naniar 패키지이용 결측치 현황파악
# install.packages('naniar')
library(naniar)

n_miss(my) # NA갯수
n_complete(my) # 시그널갯수

miss_var_summary(my) # 변수별 NA현황
miss_case_summary(my) # 레코드별 NA현황

vis_miss(my) # NA패턴 시각화


# 데이터셋 단위에 포함된 결측치 일괄 제거
# --------------------------------------------------
# 원본데이터셋의 NA현황 재확인
summary(my)
dim(my)
anyNA(my)
my %>% is.na %>% sum

# NA포함 레코드/변수 일괄 삭제 전처리
# stats::na.omit()함수이용
my_omt <- na.omit(my)
anyNA(my_omt)
my_omt %>% 
  is.na %>% sum
dim(my_omt)


# stats::complete.cases()함수이용
my_cpt <- complete.cases(my)
anyNA(my_cpt)
my_cpt %>% is.na %>% sum
dim(my_cpt)


# 특정변수의 NA값을 임의값으로 단순대체
# --------------------------------------------------
# 기저핵(barenuc)변수 NA현황
anyNA(my$barenuc)
my$barenuc %>% is.na %>% sum

# 기저핵(barenuc)변수의 NA를 대체할 대표치 산정
# 0이나평균값, 중앙값, 최빈값을 활용함
barenuc_avg <- my$barenuc %>% 
  mean(na.rm =TRUE) %>% print
barenuc_med <- my$barenuc %>% 
  median(na.rm =TRUE) %>% print
barenuc_mode <- my$barenuc %>% table %>%    
  which.max %>% names %>% as.integer %>% print

# 기저핵(barenuc)변수의 NA를 최빈값으로 대체
my$barenuc[is.na(my$barenuc)] <- barenuc_mode

# 기저핵(barenuc)변수의 NA값 대체결과 확인
anyNA(my$barenuc)
my$barenuc %>% is.na %>% sum


# 데이터셋의 전체변수들에 포함된 결측치를 임의의 값으로 일괄 대체
# --------------------------------------------------
# 각 변수별 NA값을 각 변수별 중앙값으로 일괄 단순대체
sapply(my, median, na.rm = TRUE) # 각 변수별 중앙값 산정

# 각 변수별 중앙값을 각 변수별 NA마다 대체입력
my <- sapply(my, function(x) 
  ifelse(is.na(x), yes = median(x, na.rm = TRUE), no = x)) %>% as.data.frame


# my데이터셋에 포함된 NA값 재확인
anyNA(my)
is.na(my)
sapply(my, anyNA)

library(magrittr)
my %>% is.na %>% sum
my %>% sapply(is.na) %>% 
  colSums


# --------------------------------------------------
# 이상치 처리 
# --------------------------------------------------


# 박스플롯 이용 분포현황 파악
# --------------------------------------------------
boxplot(my[1:9], main = '임상조직 세포특성 박스플롯 분포')


# 연속형 변수중 단일상피세포크기(cellsize) 관련 변수 기술통계파악 
Hmisc::describe(my$cellsize)


# 박스플롯팅
bp <- boxplot(my$cellsize, main = '단일상피세포크기(cellsize) 분포현황')

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


# 단일상피세포크기(cellsize) 변수 이상치 처리 
# --------------------------------------------------
# 상한선(upper fence) 값보다 큰 값을 절단화(trimming)
library(dplyr)

my_trim <- my %>% filter(cellsize <= bp$stats[5])
# - 원본 데이터셋에서 cellsize변수값이 상한선보다 낮은 값으로만
#   레코드를 선정해서 별도의 서브셋을 만들어 줌

# 이상치처리한 서브셋 기술통계량 확인
psych::describe(my_trim$cellsize)
Hmisc::describe(my_trim$cellsize)


# 상한선(upper fence) 값보다 큰 값을 상한선값으로 조정화(winsorizing)
my$cellsize_ws <- ifelse (my$cellsize > bp$stats[5], 
                          yes = bp$stats[5], 
                          no = my$cellsize)
# - 원본 데이터셋에서 ceelsize변수값이 상한선보다 큰경우에 
#   상한선 값으로 변경해서 원본데이터셋에 파생변수로 추가함

# 이상치처리한 파생변수 기술통계량 확인
psych::describe(my$cellsize_ws)
Hmisc::describe(my$cellsize_ws)


# --------------------------------------------------
# 스케일링 
# --------------------------------------------------

# 정규화(normalization): 센터링옵션(centering) 
# ==> -1 ~ +1 사이로 변환, ctr: centering
my_centering <- my %>% scale(center = TRUE)
summary(my_centering)
psych::describe(my_centering)

# 정규화(normalization): 레인징옵션(ranging) 
# ==> 0 ~ 1 사이로 변환, rng: ranging
my_ranging <- my %>% scale(center = FALSE)
summary(my_ranging)
psych::describe(my_ranging)

# 표준화(standardization): z-스코어(z-score) 
# ==> 평균0, 표준편차가 1인 표준정규분포로 변환
# - snd: standard normal distribution
my_snd <- my %>% scale(center = TRUE, scale = TRUE)
summary(my_snd)
psych::describe(my_snd)

par(mfrow = c(3, 1))
boxplot(my_centering, 
        main = '정규화(normalization): 센터링옵션(centering)')
boxplot(my_ranging, 
        main = '정규화(normalization): 레인징옵션(ranging)')
boxplot(my_snd, 
        main = '표준화(standardization): z-스코어(z-score)')
par(mfrow = c(1, 1))


# --------------------------------------------------
# 범주형 변수 리코딩 하기 
# --------------------------------------------------

# 판정결과변수 탐색
table(my$class)
Hmisc::describe(my$class)

# 판정결과 변수인 class 변수컬럼 레이블 반영
library(doBy)
my$class_c <- recodeVar(my$class,
                        src = c(2, 4), 
                        tgt = c("benign", "malignant")) 
# - benign: 양성, malignant: 악성


# 리코딩한 변수 기술통계 분석
class_freq <- table(my$class_c) %>% 
  sort(decreasing = TRUE) %>% head(5) %>%  print

class_prop <- class_freq %>% 
  prop.table %>% sort(decreasing = TRUE) %>% print

class_df <- data.frame(freq = c(class_freq), 
                       pect = c(round(class_prop*100, 3))) %>% print

# janitor::tabyl() 함수이용
library(janitor)
tabyl(my$class_c, sort = TRUE) %>% print

# epiDisplay::tab1() 함수이용
library(epiDisplay)
tab1(my$class_c, sort.group = "decreasing", 
     cum.percent = TRUE, main = 'class(판정결과) 분포')


# --------------------------------------------------
# 연속형 변수의 구간화를 통한 파생변수 만들기
# --------------------------------------------------

# 변수리코딩
# doBy::recodeVar() 함수이용
library(doBy)

num <- list(1:2, 3:5, 6:8, 9:10)
check <- list('저위험', '중위험', '고위험', '정밀진단')

my$thick_c <- recodeVar(my$thick,
                        src = num, 
                        tgt = check)

# 리코딩한 변수 빈도수 분석
thick_c_freq <- table(my$thick_c) %>% print
# - 위험위 순서가 고위험 > 저위험 > 정밀진단 > 중위험 순으로 나와서 
#   해석하기에 어려움

# 리코딩을 위험수준별 순서대로 변환
my$thick_f <- factor(my$thick, levels = c(1:2, 3:5, 6:8, 9:10),
                     labels = c('저위험', '저위험', '중위험', '중위험', '중위험',
                                '고위험', '고위험', '고위험', '정밀진단', '정밀진단'))

# 리코딩한 변수 빈도수 분석
thick_f_freq <- table(my$thick_f) %>% print
# - 위험의 순서가 저위험 > 중위험 > 고위험 > 정밀진단 순으로 나와서 해석하기 용이함

# 리코딩변수 비율 분석
thick_prop <- thick_f_freq %>% prop.table %>% print

# 리코딩변수 빈도와 비율값 데이터프레임으로 출력
thick_df <- data.frame(freq = c(thick_f_freq), 
                       pect = c(round(thick_prop*100, 3))) %>% print


# janitor::tabyl() 함수이용
library(janitor)
tabyl(my$thick_f) %>% print

# epiDisplay::tab1() 함수이용
library(epiDisplay)
tab1(my$thick_f, 
     cum.percent = TRUE, main = 'thick(덩어리두께) 분포')


# --------------------------------------------------
# 임상실험 샘플 수집시기 파생변수화
# --------------------------------------------------

# 유방암진단 임상실험 샘플 수집시기(UCI코딩북)
# - Group 1: 367 instances (January 1989) 
# - Group 2: 70 instances (October 1989) 
# - Group 3: 31 instances (February 1990) 
# - Group 4: 17 instances (April 1990) 
# - Group 5: 48 instances (August 1990) 
# - Group 6: 49 instances (Updated January 1991) 
# - Group 7: 31 instances (June 1991) 
# - Group 8: 86 instances (November 1991
                       

# 수집시기별 샘플수 산정
n1989 <- c(367, 70) %>% sum %>% print
n1990 <- c(31, 17, 48) %>% sum %>% print
n1991 <- c(49, 31, 86) %>% sum %>% print
total <- c(n1989, n1990, n1991) %>% sum %>% print

# 파생변수생성
my$time <- rep(c(1, 2, 3), c(n1989, n1990, n1991))
Hmisc::describe(my$time)

# 파생변수 리코딩
library(doBy)
my$time_c <- recodeVar(my$time,
                       src = c(1, 2, 3), 
                       tgt = c('y89', 'y90', 'y91')) 

# janitor::tabyl() 함수이용
library(janitor)
tabyl(my$time_c, sort = TRUE) %>% print

# epiDisplay::tab1() 함수이용
library(epiDisplay)
tab1(my$time_c, 
     sort.group = "decreasing", 
     cum.percent = TRUE, 
     main = '임상실험 샘플 수집시기')



### End of Source ####################################################

