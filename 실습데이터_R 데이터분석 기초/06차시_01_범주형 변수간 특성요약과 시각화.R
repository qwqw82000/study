######################################################################
# [06차시: 다차원변수 요약과 집계] 01. 범주형 변수간 특성요약과 시각화
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
# 범주형 변수컬럼 별도 데이터셋으로 추출
# --------------------------------------------------

# 범주형 변수컬럼명 파악
ctg_names <- c('month', 'gender', 'edu', 'job', 
               'age', 'nat', 'other', 'object', 
               'member', 'accom', 'activity')

# 범주형 변수컬럼 데이터셋 추출
ctg <- my[ctg_names]


# 범주형 데이터셋 특성파악
# --------------------------------------------------
# 간단 조회
head(ctg, 3)

# 내부구조 조회
str(ctg)

# 기본 기술통계량 파악
summary(ctg)


# --------------------------------------------------
# 성별(gender)에 따른 연령대(age) 차이분석
# --------------------------------------------------

# 성별(gender) 변수 기본특성 파악
# --------------------------------------------------

# 성별변수 간단조회
str(ctg$gender)

# 성별변수 팩터형으로 변환
ctg$gender_f <- factor(ctg$gender, levels = c(1, 2),
                       labels = c('M', 'F'))

# 성별변수 간단기술통계
summary(ctg$gender_f)


# 연령대(age) 변수 기본특성 파악
# --------------------------------------------------
# 연령대변수 간단조회
str(ctg$age)

# 연령대변수 팩터형으로 변환
ctg$age_f <- factor(ctg$age, 
                    levels = c(0, 1, 2, 3, 4, 5), 
                    labels = c('5~20세', '21~30세', 
                               '31~40세', '41~50세', 
                               '51~60세', '61세이상'))

# 연령대변수 간단기술통계
summary(ctg$age_f)


# 성별(gender)변수와 연령대(age) 변수간 관련성 파악
# --------------------------------------------------
# 팩터형 변환한 현재 데이터셋 조회
head(ctg)
str(ctg)

# 두 변수간 교차빈도분석(cross table analysis)
# - 먼저입력한 변수가 독립변수, 나중에 입력한 변수가 종속변수로 가정

# 성별(gender)에 따른 연령대(age)간 차이 교차빈도분석 
table(ctg$gender, ctg$age)
table(ctg$gender_f, ctg$age_f)
table(ctg$gender_f, ctg$age_f, 
      useNA = 'ifany')

gd_ag_freq <- table(ctg$gender_f, 
                    ctg$age_f)
gd_ag_freq # 교차분석내용 객체저장 

# 연령대(age)에 따른 성별(gender)간 차이 교차빈도분석 
table(ctg$age, ctg$gender)
table(ctg$age_f, ctg$gender_f)
table(ctg$age_f, ctg$gender_f, 
      useNA = 'ifany')

ag_gd_freq <- table(ctg$age_f, 
                    ctg$gender_f)
ag_gd_freq # 교차분석내용 객체저장 


# 성별(gender)에 따른 연령대(age)간 차이분석
# --------------------------------------------------

# 교차빈도분석 부분합(margin) 계산하기
addmargins(gd_ag_freq)
addmargins(gd_ag_freq, 1)
addmargins(gd_ag_freq, 2)

gd_ag_freq_sum <- addmargins(gd_ag_freq, 2)
gd_ag_freq_sum

# 교차빈도분석을 비율분석으로 변환
prop.table(gd_ag_freq, 1) # 각 성별에서 연령대 분포비율 비교
prop.table(gd_ag_freq, 2) # 각 연령대에서 성별 분포비율 비교

gd_ag_prop <- prop.table(gd_ag_freq, 1) 
gd_ag_prop 

addmargins(round(gd_ag_prop, 3), 2)

# 교차비율분석을 백분율분석으로 변환
gd_ag_result <- round(gd_ag_prop, 3) * 100
gd_ag_result

addmargins(gd_ag_result, 2)


# 성별(gender)변수와 연령대(age) 변수간 관련성 시각화: 빈도수 기준
# --------------------------------------------------

### 멀티 캔버스 프레임을 이용한 그래프간 비교
par(mfrow=c(2, 2))

# 누적형, 스택형, 롱포맷
barplot(gd_ag_freq,
        main="외래관광객 연령대에 따른\n 성별 분포비교: Stacked",
        xlab="연령대(age)", ylab="응답자수",
        col=c("lightblue", "pink"),
        legend=rownames(gd_ag_freq))

# 그룹형, 언스택형, 숏포맷, 와이드포맷
barplot(gd_ag_freq,
        main="외래관광객 연령대에 따른\n 성별 분포비교: Grouped",
        xlab="연령대(age)", ylab="응답자수",
        col=c("lightblue", "pink"),
        legend=rownames(gd_ag_freq), beside=TRUE)

# 누적형, 스택형, 롱포맷
barplot(ag_gd_freq,
        main="외래관광객 성별에 따른\n 연령대 분포비교: Stacked",
        xlab="성별(gender)", ylab="Frequency",
        col=rainbow(6),
        legend=rownames(ag_gd_freq))

# 그룹형, 언스택형, 숏포맷, 와이드포맷
barplot(ag_gd_freq,
        main="외래관광객 성별에 따른\n 연령대 분포비교: Grouped",
        xlab="Treatment", ylab="Frequency",
        col=rainbow(6),
        legend=rownames(ag_gd_freq), beside=TRUE)

par(mfrow=c(1, 1)) # 멀티 캔버스 프레임 리셋


# 성별(gender)변수와 연령대(age) 변수간 관련성 시각화: 비율 기준
# --------------------------------------------------
### 멀티 캔버스 프레임을 이용한 그래프간 비교
par(mfrow=c(2, 2))

plot(gender_f ~ age_f, data = ctg,
     main="외래관광객 연령대에 따른\n 성별 분포비교",
     xlab="연령대(age)", ylab="성별(gender)",
     col = rainbow(length(unique(ctg$gender_f))))

plot(age_f ~ gender_f, data = ctg,
     main="외래관광객 성별에 따른\n 연령대 분포비교",
     xlab="성별(gender)", ylab="연령대(age)",
     col = rainbow(length(levels(ctg$age_f))))

mosaicplot(age_f ~ gender_f, data = ctg,
           main="외래관광객 연령대에 따른\n 성별 분포비교",
           xlab="연령대(age)", ylab="성별(gender)",
           col = rainbow(length(unique(ctg$gender_f))))

mosaicplot(gender_f ~ age_f, data = ctg,
           main="외래관광객 성별에 따른\n 연령대 분포비교",
           xlab="성별(gender)", ylab="연령대(age)",
           col = rainbow(length(levels(ctg$age_f))))

par(mfrow=c(1, 1)) # 멀티 캔버스 프레임 리셋


# --------------------------------------------------
# 성별(gender)에 따른 다른 범주변수간 차이분석
# --------------------------------------------------

# 성별(gender)변수와 만족활동(activity) 변수간 관련성
# --------------------------------------------------
# 만족활동(activity) 변수 팩터화
ctg$activity_f <- factor(ctg$activity, levels = c(1:23),
                       labels = c('쇼핑', '식도락', '온천·스파', '휴양·휴식', 
                                  '뷰티관광', '의료관광', '유흥·오락', '카지노',
                                  '테마파크', '스포츠활동', '레포츠활동', 
                                  '직업적스포츠활동', '시찰(산업시설등)', 
                                  '연수·교육·연구', '미팅·회의·학술대회·박람회참가', 
                                  '업무수행', '고궁·역사유적지방문', '자연경관감상', 
                                  '공연·민속행사·축제참가관람', '박물관·전시관방문', 
                                  '시티투어버스이용', '전통문화체험', '기타'))

# 성별과 만족활동간 교차빈도분석
gd_ac_freq <- table(ctg$gender_f, ctg$activity_f)
gd_ac_freq
t(gd_ac_freq)


# 성별(gender)변수와 동반자유형(member) 변수간 관련성
# --------------------------------------------------
# 동반자유형(member) 변수 팩터화
ctg$member_f <- factor(ctg$member, levels = c(1:5),
                       labels = c('혼자', '가족·친지', '친구·연인', 
                                  '직장동료', '기타'))

# 성별과 동반자유형간 교차빈도분석
gd_mb_freq <- table(ctg$gender_f, ctg$member_f)
gd_mb_freq
t(gd_mb_freq)


### End of Source ####################################################

