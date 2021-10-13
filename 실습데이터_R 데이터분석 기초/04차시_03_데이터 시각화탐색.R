######################################################################
# [04차시: 분석과제 모델링과 데이터 기본탐색] 03. 데이터 시각화탐색
######################################################################

# --------------------------------------------------
# 데이터셋 로딩
# --------------------------------------------------

# 플레인텍스트파일 로딩
# --------------------------------------------------

setwd("C:/Rtest")

# utils::read.csv()함수로 플레인텍스트파일  로딩
my <- read.csv(file = 'tour.csv', 
               header = TRUE, sep = ',',
               stringsAsFactors = TRUE, 
               strip.white = TRUE,
               na.strings = c('.', '?', 'NA'))
my
class(my)
str(my)


# --------------------------------------------------
# 데이터셋 변수컬럼간 관계파악
# --------------------------------------------------

# 전체변수간 산점도 매트릭스
# --------------------------------------------------
#par("mar")
#par(mar=c(1,1,1,1))

plot(my)


# 전체변수간 상관관계 매트릭스
# --------------------------------------------------
round(cor(my), 3)
# - 일부 NA포함변수는 출력안됨

# 상관관계결과를 넘겨받아서 소수자리 정리
temp <- cor(my)
round(temp, 3)

# 데이터셋의 NA요소 제거후 상관관계 분석
temp <- cor(my, 
            use = 'complete.obs')
round(temp, 2)


# 범주형변수간 산점도 매트릭스
# --------------------------------------------------
# 성별과 연령대 간 관련성
plot(my$gender, my$age)
cor(my$gender, my$age, method = 'spearman')
cor(my$gender, my$age, method = 'spearman', use = 'complete.obs')

# 성별과 여행목적 간 관련성
plot(my$gender, my$object)
cor(my$gender, my$object, method = 'spearman')


# 범주형 변수 서브셋 추출
# 성별, 연령대, 여행목적, 숙소유형
ctg <- my[c('gender', 'age', 'object', 'accom')]
str(ctg)

# 성별, 연령대, 여행목적, 숙소유형 간 관련성
plot(ctg)
cor(ctg, method = 'spearman')
cor(ctg, method = 'spearman', use = 'complete.obs')


# 연속형변수간 산점도 매트릭스
# --------------------------------------------------
# 방문후 이미지와 재방문의지 간 관련성
plot(my$af_img, my$revisit)
cor(my$af_img, my$revisit, method = 'pearson')

# 방문후 이미지와 추천의지 간 관련성
plot(my$af_img, my$recom)
cor(my$af_img, my$recom, method = 'pearson')

# 연속형 변수 서브셋 추출 
# 방문후이미지, 재방문의지, 추천의지 서브셋 추출
cnt <- my[c('af_img', 'revisit', 'recom')]
str(cnt)

# 방문후이미지, 재방문의지, 추천의지 간 관련성
plot(cnt)
cor(cnt, method = 'pearson')


# 범주-연속 형변수간 산점도 매트릭스
# --------------------------------------------------
# 동반자유형과 동반자수 간 관련성
plot(my$member, my$number)
cor(my$member, my$number, method = 'spearman')

# 여행목적과 체류기간 간 관련성
plot(my$object, my$period)
cor(my$object, my$period, method = 'spearman')


# --------------------------------------------------
# 산점도매트릭스 패키지 이용
# --------------------------------------------------

# 범주형 변수 서브셋: 성별, 연령대, 여행목적, 숙소유형
str(ctg)
head(ctg)

# 연속형 변수 서브셋: 방문후이미지, 재방문의지, 추천의지 서브셋 추출
str(cnt)
head(cnt)

# 산점도매트릭스 패키지 동시설치
# install.packages(c('psych', 'PerformanceAnalytics', 'corrplot'))

# psych::pairs.panels() 함수이용
# --------------------------------------------------
library(psych)

pairs.panels(ctg,  
             method = "spearman",
             hist.col = "green",
             density = TRUE,
             ellipses = TRUE)

pairs.panels(cnt,  
             method = "pearson",
             hist.col = "green",
             density = TRUE,
             ellipses = TRUE)


# PerformanceAnalytics::chart.Correlation() 함수이용
# --------------------------------------------------
library(PerformanceAnalytics)

chart.Correlation(ctg, histogram = T)
chart.Correlation(cnt, histogram = T)


# corrplot::corrplot() 함수이용
# --------------------------------------------------
library(corrplot)

# NA포함 데이터관찰치 제외
ctg_pure <- na.omit(ctg)
cnt_pure <- na.omit(cnt)
my_pure <- na.omit(my)

# 범주형 변수간 연관관계 파악
corrplot(cor(ctg))
corrplot.mixed(cor(ctg))

corrplot(cor(ctg_pure), method = 'pie')
corrplot.mixed(cor(ctg_pure))


# 연속형 변수간 상관관계 파악
corrplot(cor(cnt))
corrplot.mixed(cor(cnt))

corrplot(cor(cnt_pure), method = 'pie')
corrplot.mixed(cor(cnt_pure))

# 전체 변수간 관련성 매트릭스
corrplot(cor(my))
corrplot.mixed(cor(my_pure))


### End of Source ####################################################

