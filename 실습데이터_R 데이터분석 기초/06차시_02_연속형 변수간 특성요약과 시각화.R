######################################################################
# [06차시: 다차원변수 요약과 집계] 02. 연속형 변수간 특성요약과 시각화
######################################################################

 
# --------------------------------------------------
# 데이터셋 로딩
# --------------------------------------------------

# 플레인텍스트파일 로딩
# --------------------------------------------------
# utils::read.csv()함수로 플레인텍스트파일  로딩
my <- read.csv(file = 'tour.csv', 
               header = TRUE, sep = ',',
               stringsAsFactors = TRUE, 
               strip.white = TRUE,
               na.strings = c('.', '?', 'NA'))

# 전체 데이터셋 특성파악
my
head(my)
class(my)
str(my)


# --------------------------------------------------
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
cnt_names <- setdiff(all_names, ctg_names)
cnt_names

# 범주형 변수컬럼 데이터셋 추출
cnt <- my[cnt_names]
cnt <- cnt[-1] # 첫번째 변수컬럼인 id변수 제외


# 연속형 데이터셋 특성파악
# --------------------------------------------------
# 간단 조회
head(cnt, 3)

# 내부구조 조회
str(cnt)

# 기본 기술통계량 파악
summary(cnt)

library(skimr)
skim(cnt)


# --------------------------------------------------
# 방문후이미지(af_img)에 따른 재방문의지(revisit) 상관성 분석
# --------------------------------------------------

# 방문후이미지(af_img) 변수 기본특성 파악
# --------------------------------------------------
# 방문후이미지 변수 간단조회
str(cnt$af_img)

# 방문후이미지변수 간단기술통계
library(psych)
psych::describe(cnt$af_img)

library(skimr)
skim(cnt$af_img)

# 재방문의지(revisit) 변수 기본특성 파악
# --------------------------------------------------
# 재방문의지 변수 간단조회
str(cnt$revisit)

# 재방문의지 변수 간단기술통계
library(psych)
psych::describe(cnt$revisit)

library(skimr)
skim(cnt$revisit)


# 방문후이미지(af_img)에 따른 재방문의지(revisit) 변수간 상관관계 파악
# --------------------------------------------------
# 공분산(covariance) 분석
var(cnt$af_img, cnt$revisit)

# 상관성(correlation) 분석
cor(cnt$af_img, cnt$revisit, method = 'spearman')
cor(cnt$af_img, cnt$revisit, method = 'pearson')


# 방문후이미지(af_img)에 따른 재방문의지(revisit) 변수간 기본산점도
# --------------------------------------------------
# 기본 plot() 함수이용: 직선과 곡선 최적합화선 추가
plot(revisit ~ af_img, data = cnt, pch=19,
     main = '방문후이미지가 재방문의지에 미치는 영향',
     xlab = '방문후이미지(af_img)', 
     ylab = '재방문의지(revisit)')

# 최적의 추세직선추가
abline(lm(revisit ~ af_img, data = cnt), 
       col = "red", lwd = 2, lty = 1) 

# 최적의 추세곡선추가
lines(lowess(cnt$revisit ~ cnt$af_img), 
      col = "blue", lwd = 2, lty = 2) 


# --------------------------------------------------
# 카드사용액(card)과 현금사용액(cash)간 상관성 분석
# --------------------------------------------------

# 카드사용액(card) 변수 기본특성 파악
# --------------------------------------------------
# 카드사용액 변수 간단조회
str(cnt$card)

# 카드사용액 간단기술통계
library(psych)
psych::describe(cnt$card)


# 현금사용액(cash) 변수 기본특성 파악
# --------------------------------------------------
# 현금사용액 변수 간단조회
str(cnt$cash)

# 현금사용액 변수 간단기술통계
library(psych)
psych::describe(cnt$cash)


# 카드사용액(card)에 따른 현금사용액(cash) 변수간 상관관계 파악
# --------------------------------------------------
# 공분산(covariance) 분석
var(cnt$card, cnt$cash)

# 상관성(correlation) 분석
cor(cnt$card, cnt$cash, method = 'spearman')
cor(cnt$card, cnt$cash, method = 'pearson')


# 카드사용액(card)에 따른 현금사용액(cash) 변수간 기본산점도
# --------------------------------------------------
# 기본 graphics::plot() 함수이용: 직선과 곡선 최적합화선 추가
plot(cash ~ card, data = cnt, pch=19,
     main = '신용카드사용액과 현금사용액간 관련성',
     xlab = '카드사용액(card)', 
     ylab = '현금사용액(cash)')

# 최적의 추세직선추가
abline(lm(cash ~ card, data = cnt), 
       col="red", lwd=2, lty=1)

# 최적의 추세곡선추가
lines(lowess(cnt$cash ~ cnt$card), 
      col="blue", lwd=2, lty=2) 


# ggplot2::ggplot() 함수이용: 최적합선 추가
# --------------------------------------------------
library(ggplot2)

p <- ggplot(data = cnt, aes(x = card, y = cash)) +
  geom_point() + 
  labs(title = "신용카드사용액과 현금사용액간 관련성", 
       x = "카드사용액(card)", 
       y = "현금사용액")
p

p <- p + geom_smooth() # 최적합선과 오차범위를 표현
p

# plotly::plotly() 함수이용: 최적합선 추가
# --------------------------------------------------
# install.packages('plotly')
library(plotly)

ggplotly(p) 
# - 브라우저로 인터넷사이트에 접속한 것처럼
#   그래프 위에 마우스 커서를 가져가면
#   특정 위치별 x, y값이 동적으로 확인가능함



### End of Source ####################################################




