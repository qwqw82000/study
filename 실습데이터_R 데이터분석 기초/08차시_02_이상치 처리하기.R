######################################################################
# [08차시: 데이터 정제] 02. 이상치 처리하기
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

summary(my)


# --------------------------------------------------
# 범주형 변수 이상치 검출
# --------------------------------------------------

# 범주형 변수 중 논리적으로 조작적 정의내용에 부합하지 않는 변수 탐색 
# --------------------------------------------------

# 직업유형(job) 변수 조작적정의 
# - 외래관광객 직업유형을 12가지로 조사함
# - 1: 공무원·군인, 2: 기업인·경영직, 3: 사무·기술직, 4: 판매·서비스직, 
# - 5: 전문직, 6: 생산·기능·노무직, 7: 자영업자, 8: 학생, 9: 주부, 
# - 10: 은퇴자, 11: 무직, 12: 기타 

# 직업유형(job) 변수 기본탐색
str(my$job)
library(Hmisc)
Hmisc::describe(my$job)
# - 1번직업~12번직업 이외에 다른 직업유형이 보임


# 직업유형(job)변수 원본 수치형에서 문자형으로 리코딩
# --------------------------------------------------
# plyr::mapvalues() 함수이용
# install.packages('plyr')
library(plyr)

src <- c(1:12, 13, 14, 17)
tgt <- c('공무원·군인', '기업인·경영직', '사무·기술직', 
         '판매·서비스직', '전문직', '생산·기능·노무직', 
         '자영업자', '학생', '주부', '은퇴자', 
         '무직', '기타', 13, 14, 17)

my$job_c <- plyr::mapvalues(my$job, 
                            from = src, to = tgt)

# 리코딩결과 변수컬럼명으로 확인
head(my[c('job', 'job_c')])


# 직업유형(job)변수 문자형으로 리코딩결과 기술통계분석
# --------------------------------------------------
library(magrittr)
my$job_c %>% table %>% print %>% 
  sort(decreasing = TRUE) %>% as.data.frame %>% View

Hmisc::describe(my$job_c)
# - 이상치가 13, 14, 17인데, 14는 발생건수가 4개라 무시해도됨
# - 13과 17은 10개 이상 발생했으므로 별도 서브셋을 통해 원인파악 필요


# 직업유형(job)변수의 이상치데이터를 서브셋으로 추출
# --------------------------------------------------
# - 어떠한 관찰치가 이상치로 기록되었는지 프로파일링
job_ot <- subset(my, 
                 job %in% c(13, 14, 17),
                 select = c('id', 'month', 'gender', 'age', 'job_c'))

head(job_ot)
Hmisc::describe(job_ot)


# 직업유형(job) 이상치와 조사시기(month) 간 교차분석
# --------------------------------------------------
library(magrittr)
table(job_ot$month, job_ot$job_c) %>% addmargins
# - 13과 14는 조사시기에 따라 무작위로 발생하고 있어 무시해도됨
# - 그러나 6월달에 17번이라는 응답치가 11개 발생함
# - 6월달 조사에서 왜 이런 패턴이 두드러지게 나왔는지
#   당시 조사방법이나 조사요원, 조사대상 등에 대해 리뷰해보아야 함
# - 또한 서베이 내용을 PC로 옮길 때 1번이나 3번을 13번으로, 
#   1번이나 7번을 17번으로 잘못 코딩할 수도 있음


# --------------------------------------------------
# 범주형 변수 이상치 처리
# --------------------------------------------------

# 직업유형(job) 변수의 이상치 처리방향
# --------------------------------------------------
# - 13번 이상치는 단순 결측값으로 표기
# - 14번 이상치는 10개 정도이므로 12번 기타유형으로 대체 
# - 17번 유형은 26개 규모가 있으므로, 직업유형 중 최빈치로 대체

# 직업유형변수 중 최빈치 파악
my$job %>% table %>% 
  sort(decreasing = TRUE) %>% which.max %>% names
my$job_c %>% table %>% 
  sort(decreasing = TRUE) %>% which.max %>% names

# 직업유형(job) 변수의 이상치 처리실시
library(plyr)
my$job <- plyr::mapvalues(my$job, 
                            from = c(13, 14, 17), to = c(12, NA, 3))

my$job_c <- plyr::mapvalues(my$job_c, 
                            from = c(13, 14, 17), 
                            to = c('기타', NA, '사무·기술직'))

# 리코딩결과 변수컬럼명으로 확인
head(my[c('job', 'job_c')])


# 직업유형(job) 변수의 이상치 처리내용 확인
# --------------------------------------------------
table(my$job, useNA = 'ifany')
table(my$job_c, useNA = 'ifany')



# --------------------------------------------------
# 연속형 변수 이상치 검출
# --------------------------------------------------

# utils::read.csv()함수로 로딩
my <- read.csv(file = 'tour.csv', 
               header = TRUE, sep = ',',
               stringsAsFactors = FALSE, 
               strip.white = TRUE,
               na.strings = c('.', '?', 'NA'))


# 연속형 변수컬럼 별도 데이터셋으로 추출
# --------------------------------------------------

# 전체 변수컬럼명 파악
all_names <- names(my)
all_names

# 범주형 변수컬럼명 파악
ctg_names <- c('month', 'gender', 'edu', 'job', 
               'age', 'nat', 'other', 'object', 
               'member', 'accom', 'activity')

# 연속형 변수컬럼명 도출
cnt_names <- setdiff(all_names, c(ctg_names, 'id'))
cnt_names

# 연속형 변수컬럼으로 구성된 서브셋 도출
cnt <- my[cnt_names]


# 연속형 데이터셋 특성파악
# --------------------------------------------------
# 간단 조회
head(cnt, 3)

# 내부구조 조회
str(cnt)

# 기본 기술통계량 파악
library(skimr)
skim(cnt)


# 박스플롯을 이용한 일변량/단변량(개별 연속형변수) 이상점 검출 
# --------------------------------------------------

# 연속형 변수 전체 박스플롯을 통한 이상치 탐지
boxplot(cnt)
# - 비용관련 변수의 범위가 너무 극단치라서 주제별로 박스플롯을 세분화 필요

# 연속형 변수중 비용관련 변수 박스플롯
boxplot(cnt[c('expense', 'card', 'cash')])

# 연속형 변수중 만족도관련 변수 박스플롯
boxplot(cnt[c("overal", "bf_img", "af_img", "revisit", "recom")])

# 연속형 변수중 나머지 관광행태 관련변수 박스플롯
boxplot(cnt[c("decision", "count", "number", "period")])


# 박스플롯을 이용한 지출경비(expense) 변수 이상점 검출 
# --------------------------------------------------
# 연속형 변수중 지출경비(expense) 관련 변수 기술통계파악 
Hmisc::describe(cnt$expense)
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


### End of Source ####################################################

