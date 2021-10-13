######################################################################
# [13차시: 건강정보 데이터 분석] 03. 변수간 관계특성 분석
######################################################################
 

# --------------------------------------------------
# 데이터셋 준비
# --------------------------------------------------

# 데이터셋 파일 로딩
# --------------------------------------------------

# readr::read_delim() 함수이용 티블형식으로 로딩
library(readr)
my <- read_delim(file = 'cancer.csv', col_names = TRUE, 
                 delim = ',', trim_ws = TRUE, 
                 na = c('.', '?', 'NA')) 

# 간단 데이터셋 파악
my
summary(my)
str(my)


# --------------------------------------------------
# 결측값 처리
# --------------------------------------------------

# 변수컬럼별 결측값 규모파악
library(magrittr)
my %>% sapply(is.na) %>% colSums

# 각 변수별 중앙값 파악
sapply(my, median, na.rm = TRUE)

# 각 변수별 중앙값을 각 변수별 NA마다 대체입력
my <- sapply(my, function(x) 
  ifelse(is.na(x), yes = median(x, na.rm = TRUE), no = x)) %>% as.data.frame

# 변수컬럼별 결측값 규모 재파악
my %>% sapply(is.na) %>% colSums


# --------------------------------------------------
# 변수 리코딩
# --------------------------------------------------


# 종양유형(class) 변수 리코딩
# --------------------------------------------------

# 종양유형변수 탐색
table(my$class)
Hmisc::describe(my$class)

# 종양유형 변수인 class 변수컬럼 레이블 반영
library(doBy)
my$class_c <- recodeVar(my$class,
                        src = c(2, 4), 
                        tgt = c("양성", "악성")) 
# - benign: 양성, malignant: 악성


# 리코딩한 변수 기술통계 분석
# --------------------------------------------------
# 빈도수 계산
class_freq <- table(my$class_c) %>% 
  sort(decreasing = TRUE) %>% head(5) %>%  print

# 비율 계산
class_prop <- class_freq %>% 
  prop.table %>% sort(decreasing = TRUE) %>% print

# 빈도수와 비율을 데이터프레임으로 조회
class_df <- data.frame(freq = c(class_freq), 
                       pect = c(round(class_prop*100, 3))) %>% print

# 기술통계 분석
# janitor::tabyl() 함수이용
library(janitor)
tabyl(my$class_c, sort = TRUE) %>% print

# epiDisplay::tab1() 함수이용
library(epiDisplay)
tab1(my$class_c, sort.group = "decreasing", 
     cum.percent = TRUE, main = 'class(종양유형) 분포')


# --------------------------------------------------
# 종양유형(class)에 따른 임상조직 세포특성차이 분석
# --------------------------------------------------

# stats::aggregate() 이용 요약집계 작업
# --------------------------------------------------
# 요약집계 기준변수: 종양유형(class) 1개 
# 요약집계 결과변수: 암세포특성변수 1개씩 설정
# 요약집계 통계함수: 평균(mean) 1개

aggregate(thick ~ class_c, my, mean)
aggregate(thick ~ class_c, my, mean, na.rm = TRUE)
aggregate(thick ~ class_c, my, mean, trim = 0.05)
aggregate(thick ~ class_c, my, mean, na.rm = TRUE, trim = 0.05)

aggregate(unisize ~ class_c, my, mean)
aggregate(unishape ~ class_c, my, mean)
aggregate(adhesion ~ class_c, my, mean)
aggregate(cellsize ~ class_c, my, mean)
aggregate(barenuc ~ class_c, my, mean)
aggregate(chromatin ~ class_c, my, mean)
aggregate(normnuc ~ class_c, my, mean)
aggregate(mitosis ~ class_c, my, mean)


# for 반복구문을 이용한 1:1 요약집계 자동화
names(my)

for (i in 2:9) {
  aggregate(x = list(my[i]), 
            by = list(my$class_c), 
            FUN = mean) %>% print
  cat('\n')
}


# stats::aggregate() 이용 요약집계 작업
# --------------------------------------------------
# 요약집계 기준변수: 종양유형(class) 1개 
# 요약집계 결과변수: 암세포특성변수 2개 이상 설정
# 요약집계 통계함수: 평균(mean) 1개

aggregate(cbind(thick, cellsize) ~ class_c, 
          my, mean)
aggregate(cbind(thick, cellsize) ~ class_c, 
          my, mean, na.rm = TRUE, trim = 0.05)

aggregate(cbind(unisize, unishape, thick) ~ class_c, my, mean)

aggregate(cbind(thick, unisize, unishape, 
                adhesion, cellsize, barenuc, 
                chromatin, normnuc, mitosis) ~ class_c, my, mean)


# psych::describeBy() 이용 요약집계 작업
# --------------------------------------------------
names(my)
psych::describeBy(x = my[2:10], group = my$class_c)
psych::describeBy(x = my[2:10], group = my$class_c, mat = TRUE)


# dplyr:: group_by()와 summarize() 이용 요약집계 작업
# --------------------------------------------------
library(dplyr); library(magrittr)

# 요약집계 기준변수: 종양유형(class) 1개 
# 요약집계 결과변수: 암세포특성변수 1개씩 설정
# 요약집계 통계함수: 평균(mean), 표준편차(sd) 2개
my %>% group_by(class_c) %>% 
  dplyr::summarize(thick_avg = mean(thick), 
                   thick_std = sd(thick))

# 결측치 먼저 필터링하고 연산
my %>% filter(!is.na(thick)) %>% group_by(class_c) %>% 
  dplyr::summarize(thick_avg = mean(thick), 
                   thick_std = sd(thick))

# 요약집계 기준변수: 종양유형(class) 1개 
# 요약집계 결과변수: 암세포특성변수 2개씩 설정
# 요약집계 통계함수: 평균(mean), 표준편차(sd) 2개
my %>% group_by(class_c) %>% 
  dplyr::summarize(thick_avg = mean(thick), thick_std = sd(thick),
                   unisize_avg = mean(unisize), unisize_std = sd(unisize))

# 요약집계 결과변수: 암세포특성변수 여러 개 설정
my %>% dplyr::select(thick:mitosis, class_c) %>%
  group_by(class_c) %>% dplyr::summarise_all(mean)


# --------------------------------------------------
# 종양유형(class)에 따른 임상조직 세포특성차이 시각화
# --------------------------------------------------

# ggplot2::ggplot() 함수이용
# --------------------------------------------------
library(ggplot2); library(gridExtra)

p1 <- ggplot(data = my, aes(x = class_c, y = thick, 
                            fill = class_c)) + geom_boxplot()
p2 <- ggplot(data = my, aes(x = class_c, y = unisize, 
                            fill = class_c)) + geom_boxplot()
p3 <- ggplot(data = my, aes(x = class_c, y = unishape, 
                            fill = class_c)) + geom_boxplot()
p4 <- ggplot(data = my, aes(x = class_c, y = adhesion, 
                            fill = class_c)) + geom_boxplot()
p5 <- ggplot(data = my, aes(x = class_c, y = cellsize, 
                            fill = class_c)) + geom_boxplot()
p6 <- ggplot(data = my, aes(x = class_c, y = barenuc, 
                            fill = class_c)) + geom_boxplot()
p7 <- ggplot(data = my, aes(x = class_c, y = chromatin, 
                            fill = class_c)) + geom_boxplot()
p8 <- ggplot(data = my, aes(x = class_c, y = normnuc, 
                            fill = class_c)) + geom_boxplot()
p9 <- ggplot(data = my, aes(x = class_c, y = mitosis, 
                            fill = class_c)) + geom_boxplot()


grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3)


# caret::featurePlot() 함수이용
# --------------------------------------------------
# install.packages("caret")
library(caret)

names(my)

featurePlot(x = my[ , c(2:10)], y = factor(my$class_c), 
            plot = "box",
            scales = list(y = list(relation="free"),
                          x = list(rot = 0)),
            layout = c(3, 3))

featurePlot(x = my[ , c(2:10)], y = factor(my$class_c), 
            plot = "density",
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            adjust = 1.5,
            pch = "|",
            layout = c(3, 3),
            auto.key = list(columns = 2)) # 상단범례표시


# --------------------------------------------------
# 파생변수 생성
# --------------------------------------------------

# 임상실험 샘플 수집시기(time) 파생변수화
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


# --------------------------------------------------
# 종양유형(class)과 임상시기(time)에 따른 임상조직 세포특성차이 분석
# --------------------------------------------------

# stats::aggregate() 이용 요약집계 작업
# --------------------------------------------------
# 요약집계 기준변수: 종양유형(class), 임상시기(time) 2개 
# 요약집계 결과변수: 암세포특성변수 1개씩 설정
# 요약집계 통계함수: 평균(mean) 1개

aggregate(thick ~ time_c + class_c, my, mean)
aggregate(thick ~ class_c + time_c, my, mean)

aggregate(thick ~ time_c + class_c, my, mean, na.rm = TRUE)
aggregate(thick ~ time_c + class_c, my, mean, trim = 0.05)
aggregate(thick ~ time_c + class_c, my, mean, na.rm = TRUE, trim = 0.05)

aggregate(unisize ~ time_c + class_c, my, mean)
aggregate(unishape ~ time_c + class_c, my, mean)
aggregate(adhesion ~ time_c + class_c, my, mean)
aggregate(cellsize ~ time_c + class_c, my, mean)
aggregate(barenuc ~ time_c + class_c, my, mean)
aggregate(chromatin ~ time_c + class_c, my, mean)
aggregate(normnuc ~ time_c + class_c, my, mean)
aggregate(mitosis ~ time_c + class_c, my, mean)


# for 반복구문을 이용한 1:1 요약집계 자동화
names(my)

for (i in 2:10) {
  aggregate(x = list(my[i]), 
            by = list(my$time_c, my$class_c), 
            FUN = mean) %>% print
  cat('\n')
}


# stats::aggregate() 이용 요약집계 작업
# --------------------------------------------------
# 요약집계 기준변수: 종양유형(class), 임상시기(time) 2개 
# 요약집계 결과변수: 암세포특성변수 2개 이상 설정
# 요약집계 통계함수: 평균(mean) 1개

aggregate(cbind(thick, cellsize) ~ time_c + class_c, my, mean)
aggregate(cbind(thick, cellsize) ~ time_c + class_c, 
          my, mean, na.rm = TRUE, trim = 0.05)

aggregate(cbind(unisize, unishape, thick) ~ time_c + class_c, my, mean)
aggregate(cbind(thick, unisize, unishape, 
                adhesion, cellsize, barenuc, 
                chromatin, normnuc, mitosis) ~ time_c + class_c, my, mean)


# psych::describeBy() 이용 요약집계 작업
# --------------------------------------------------
names(my)
psych::describeBy(x = my[2:10], 
                  group = list(my$time_c, 
                               my$class_c),
                  digits = 3)

psych::describeBy(x = my[2:10], 
                  group = list(my$time_c, 
                               my$class_c),
                  digits = 3, mat = TRUE)


# dplyr:: group_by()와 summarize() 이용 요약집계 작업
# --------------------------------------------------
library(dplyr); library(magrittr)

# 요약집계 기준변수: 종양유형(class), 임상시기(time) 2개 
# 요약집계 결과변수: 암세포특성변수 1개씩 설정
# 요약집계 통계함수: 평균(mean), 표준편차(sd) 2개
my %>% group_by(time_c, class_c) %>% 
  dplyr::summarize(thick_avg = mean(thick), 
                   thick_std = sd(thick))

# 결측치 먼저 필터링하고 연산
my %>% filter(!is.na(thick)) %>% 
  group_by(time_c, class_c) %>% 
  dplyr::summarize(thick_avg = mean(thick), 
                   thick_std = sd(thick))

# 요약집계 기준변수: 종양유형(class), 임상시기(time) 2개 
# 요약집계 결과변수: 암세포특성변수 2개씩 설정
# 요약집계 통계함수: 평균(mean), 표준편차(sd) 2개
my %>% group_by(time_c, class_c) %>% 
  dplyr::summarize(thick_avg = mean(thick), thick_std = sd(thick),
                   unisize_avg = mean(unisize), unisize_std = sd(unisize))

# 요약집계 결과변수: 암세포특성변수 여러 개 설정
my %>% dplyr::select(thick:mitosis, time_c, class_c) %>%
  group_by(time_c, class_c) %>% dplyr::summarise_all(mean)


# --------------------------------------------------
# 종양유형(class)과 임상시기(time)에 따른 임상조직 세포특성차이 시각화
# --------------------------------------------------

# ggplot2::ggplot() 함수이용
# --------------------------------------------------
library(ggplot2)

p1 <- ggplot(data = my, aes(x = class_c, y = thick, 
                            fill = time_c)) + geom_boxplot(show.legend = FALSE)
p2 <- ggplot(data = my, aes(x = class_c, y = unisize, 
                            fill = time_c)) + geom_boxplot(show.legend = FALSE)
p3 <- ggplot(data = my, aes(x = class_c, y = unishape, 
                            fill = time_c)) + geom_boxplot(show.legend = FALSE)
p4 <- ggplot(data = my, aes(x = class_c, y = adhesion, 
                            fill = time_c)) + geom_boxplot(show.legend = FALSE)
p5 <- ggplot(data = my, aes(x = class_c, y = cellsize, 
                            fill = time_c)) + geom_boxplot(show.legend = FALSE)
p6 <- ggplot(data = my, aes(x = class_c, y = barenuc, 
                            fill = time_c)) + geom_boxplot(show.legend = FALSE)
p7 <- ggplot(data = my, aes(x = class_c, y = chromatin, 
                            fill = time_c)) + geom_boxplot(show.legend = FALSE)
p8 <- ggplot(data = my, aes(x = class_c, y = normnuc, 
                            fill = time_c)) + geom_boxplot(show.legend = FALSE)
p9 <- ggplot(data = my, aes(x = class_c, y = mitosis, 
                            fill = time_c)) + geom_boxplot()

# 멀티캔버스 이용
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3)


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
# 종양유형(class)에 따른 임상조직 세포특성차이 교차분석
# --------------------------------------------------

# stas::xtabs() 함수이용 1:1 교차분석 작업
# --------------------------------------------------
class_thick_freq <- xtabs(~ class_c + thick_f, data = my) %>% print
addmargins(class_thick_freq, 2)

class_thick_prop <- prop.table(class_thick_freq, 1) %>% print
addmargins(class_thick_prop, 2)

class_thick_pect <- round(class_thick_prop * 100, 1) %>% print
class_thick_pect %>% addmargins(2)


# --------------------------------------------------
# 종양유형(class)과 임상시기(time)에 따른 임상조직 세포특성차이 시각화
# --------------------------------------------------


# CGPfunctions::PlotXTabs() 함수이용
# --------------------------------------------------
# install.packages('CGPfunctions')
library(CGPfunctions)

PlotXTabs(my, class_c, thick_f, 'stack')

PlotXTabs(my, class_c, thick_f, 'side')

PlotXTabs(my, class_c, thick_f, 'percent')


# ggplot2::ggplot() 함수이용
# --------------------------------------------------
library(ggplot2); library(gridExtra)

p1 <- ggplot(data = my, aes(x = class_c, fill = thick_f)) +
  geom_bar(show.legend = FALSE)

p2 <- ggplot(data = my, aes(x = class_c, fill = thick_f)) +
  geom_bar(show.legend = FALSE, position = 'dodge')

p3 <- ggplot(data = my, aes(x = class_c, fill = thick_f)) +
  geom_bar(position = 'fill')

p4 <- ggplot(data = my, aes(x = thick_f, fill = class_c)) +
  geom_bar(show.legend = FALSE)

p5 <- ggplot(data = my, aes(x = thick_f, fill = class_c)) +
  geom_bar(show.legend = FALSE, position = 'dodge')

p6 <- ggplot(data = my, aes(x = thick_f, fill = class_c)) +
  geom_bar(position = 'fill')

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)


### End of Source ####################################################

