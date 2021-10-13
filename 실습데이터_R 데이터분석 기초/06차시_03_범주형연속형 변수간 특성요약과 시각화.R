######################################################################
# [06차시: 다차원변수 요약과 집계] 03. 범주형-연속형 변수간 특성요약과 시각화
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


# 전체 변수컬럼명 파악
all_names <- names(my)
all_names

# 범주형 변수컬럼명 파악
ctg_names <- c('month', 'gender', 'edu', 'job', 
               'age', 'nat', 'other', 'object', 
               'member', 'accom', 'activity')
ctg_names

# 연속형 변수컬럼명 도출
cnt_names <- setdiff(all_names, ctg_names)
cnt_names <- setdiff(cnt_names, 'id')
cnt_names

# 측정척도별 변수컬럼 갯수
sprintf('전체변수갯수: %d개 = 범주형변수갯수(%d개) + 연속형변수갯수(%d개)',
        length(all_names), length(ctg_names), length(cnt_names))


# --------------------------------------------------
# 2차원 변수간 요약집계: 성별과 지출경비
# --------------------------------------------------

# 성별과 지출경비간 기술통계
# --------------------------------------------------
# 성별변수 팩터형으로 변환
my$gender_f <- factor(my$gender, 
                      levels = c(1, 2),
                      labels = c('M', 'F'))

# 성별변수 기술통계량
Hmisc::describe(my$gender_f)

# 지출경비 기술통계량
psych::describe(my$expense)


# 성별과 지출경비간 요약집계 실시
# --------------------------------------------------
# stats::aggregate()함수이용 요약집계
aggregate(formula = expense ~ gender, data = my, FUN = mean, na.rm = TRUE)
aggregate(expense ~ gender_f, my, mean, na.rm = TRUE, trim = 0.05)
aggregate(expense ~ gender_f, my, sd, na.rm = TRUE)

# purrr::파이프연산자%>%, dplyr::데이터가공함수 이용 요약집계  
# install.packages('purrr')
library(purrr)
library(dplyr)

my %>% 
  group_by(gender_f) %>%  
  dplyr::summarize(Avg = mean(expense), SD = sd(expense)) %>% 
  arrange(desc(Avg))


# 성별과 지출경비간 요약집계 시각화: 기본비교
# --------------------------------------------------
par(mfrow=c(1, 2))

# sm::sm.density.compare()를 사용한 밀도그래프
# install.packages("sm")
library(sm)
sm.density.compare(x=my$expense, group=my$gender_f,
                   xlab="지출경비(expense)", ylab="밀도", 
                   col=c(2, 3), lty=c(2, 3))
title(main="성별에 따른 지출경비 분포비교")
legend(x=100000, y=0.01, legend=levels(my$gender_f), 
       col=c(2, 3), lty=c(2, 3), bty="n")

# boxplot()를 사용한 그래프
boxplot(expense ~ gender_f, data = my,
        main="성별에 따른 지출경비 분포비교",
        xlab="성별(gender)", ylab="지출경비(expense)",
        col=c(2, 3), varwidth=T, notch=T)

par(mfrow=c(1, 1))


# 성별과 지출경비간 요약집계 시각화: ggplot2패키지 이용
# --------------------------------------------------
# 멀티프레임에 배치할 개별 그래프 준비
# 단순 플롯팅 그래프
p1 <- ggplot(my, aes(gender_f, expense)) + 
  geom_point(color = "red", shape = 20, size = 2)

# 모든 데이터를 플로팅하는 형태로 분포모양을 나타냄
p2 <- ggplot(my, aes(gender_f, expense)) + 
  geom_jitter(color = "blue", shape = 8, size = 0.8)

# 박스플롯 형태로 분포모양을 나타냄
p3 <- ggplot(my, aes(gender_f, expense)) + 
  geom_boxplot(fill = "lightblue", 
               outlier.color ="orange", outlier.shape = 17,
               outlier.size = 2, notch = TRUE)
# 바이올린 플롯형태로 분포모양을 나타냄
p4 <- ggplot(my, aes(gender_f, expense)) + 
  geom_violin(fill = "lightpink")

# gridExtra::grid.arrange() 함수이용 
# ggplot2용 멀티프레임 생성
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2) 


# --------------------------------------------------
# 2차원 변수간 요약집계: 연령대와 지출경비
# --------------------------------------------------

# 연령대와 지출경비간 기술통계
# --------------------------------------------------
# 연령대변수 팩터형으로 변환
my$age_f <- factor(my$age, 
                   levels = c(0, 1, 2, 3, 4, 5), 
                   labels = c('5~20세', '21~30세', 
                              '31~40세', '41~50세', 
                              '51~60세', '61세이상'))
# 연령대변수 기술통계량
Hmisc::describe(my$age_f)

# 지출경비 기술통계량
psych::describe(my$expense)

# 연령대와 지출경비간 요약집계 실시
# --------------------------------------------------
# stats::aggregate()함수이용 요약집계
aggregate(expense ~ age, my, mean, na.rm = TRUE)
aggregate(expense ~ age_f, my, mean, na.rm = TRUE, trim = 0.05)
aggregate(expense ~ age_f, my, sd, na.rm = TRUE)


# purrr::파이프연산자%>%, dplyr::데이터가공함수 이용 요약집계 
# install.packages('purrr')
library(purrr)
library(dplyr)
        
my %>% filter(!is.na(age_f)) %>% 
  group_by(age_f) %>% 
  dplyr::summarize(Avg = mean(expense), SD = sd(expense))

my %>% filter(!is.na(age_f)) %>% 
  group_by(age_f) %>% 
  dplyr::summarize(Avg = mean(expense), SD = sd(expense)) %>% 
  arrange(desc(Avg)) # 평균지출금액 기준으로 내림차순 정렬


# 연령대와 지출경비간 요약집계 시각화: 기본비교
# --------------------------------------------------
par(mfrow = c(1, 2))

# sm::sm.density.compare()를 사용한 밀도그래프
# install.packages("sm")
library(sm)
sm.density.compare(x = my$expense, group=my$age_f,
                   xlab = "지출경비(expense)", ylab = "밀도", 
                   col = c(2:7), lty = c(2:7))
title(main = "연령대에 따른 지출경비 분포비교")
legend(x = 100000, y = 0.01, legend = levels(my$age_f), 
       col = c(2:7), lty = c(2:7), bty = "n")

# boxplot()를 사용한 그래프
boxplot(expense ~ age_f, data = my,
        main = "연령대에 따른 지출경비 분포비교",
        xlab = "연령대(age) 갯수", ylab = "지출경비(expense)",
        col = c(2:7), varwidth = T, notch = T)

par(mfrow = c(1, 1))


# 연령대와 지출경비간 요약집계 시각화: ggplot2패키지 이용
# --------------------------------------------------
# 멀티프레임에 배치할 개별 그래프 준비
# 단순 플롯팅 그래프
p1 <- ggplot(my, aes(age_f, expense)) + 
  geom_point(color = "red", shape = 20, size = 2)

# 모든 데이터를 플로팅하는 형태로 분포모양을 나타냄
p2 <- ggplot(my, aes(age_f, expense)) + 
  geom_jitter(color = "blue", shape = 8, size = 0.8)

# 박스플롯 형태로 분포모양을 나타냄
p3 <- ggplot(my, aes(age_f, expense)) + 
  geom_boxplot(fill = "lightblue", 
               outlier.color ="orange", outlier.shape = 17,
               outlier.size = 2, notch = TRUE)
# 바이올린 플롯형태로 분포모양을 나타냄
p4 <- ggplot(my, aes(age_f, expense)) + 
  geom_violin(fill = "lightpink")

# gridExtra::grid.arrange() 함수이용 
# ggplot2용 멀티프레임 생성
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2) 

# plotly::plotly() 함수이용 요약집계 시각화
library(plotly)
ggplotly(p2)


# --------------------------------------------------
# 3차원 변수간 요약집계: 성별, 연령대, 지출경비
# --------------------------------------------------


# 성별과 지출경비간 요약집계를 제3의 변수인 연령대에 따라 세분화 요약집계
# --------------------------------------------------
aggregate(expense ~ gender_f, my, 
          mean, na.rm = TRUE, trim = 0.05)
aggregate(expense ~ gender_f + age_f, my, 
          mean, na.rm = TRUE, trim = 0.05)

# 연령대와 지출경비간 요약집계를 제3의 변수인 성별에 따라 세분화 요약집계
# --------------------------------------------------
aggregate(expense ~ age_f, my, 
          mean, na.rm = TRUE, trim = 0.05)
aggregate(expense ~ age_f + gender_f, my, 
          mean, na.rm = TRUE, trim = 0.05)


# 3차원 변수간 시각화
# --------------------------------------------------
# 멀티프레임에 배치할 개별 그래프 준비
p1 <- ggplot(my, aes(gender_f, expense, color = age_f, shape = age_f)) + 
  geom_boxplot() + 
  labs(title = "성별에 따른 지출금액 분포를 연령대별로 세분화해 비교분석")

p2 <- ggplot(my, aes(gender_f, expense)) + 
  geom_boxplot() + facet_wrap(~ age_f) + 
  labs(title = "성별에 따른 지출금액 분포를 연령대별로 분할그래프로 비교분석")

p3 <- ggplot(my, aes(age_f, expense, color = gender_f, shape = gender_f)) + 
  geom_boxplot() + 
  labs(title = "연령대에 따른 지출금액 분포를 성별로 세분화해 비교분석")

p4 <- ggplot(my, aes(age_f, expense)) + geom_boxplot() + 
  facet_wrap(~ gender_f) + 
  labs(title = "연령대에 따른 지출금액 분포를 성별로 분할그래프로 비교분석")


# gridExtra::grid.arrange() 함수이용 
# ggplot2용 멀티프레임 생성
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2) 


### End of Source ####################################################



