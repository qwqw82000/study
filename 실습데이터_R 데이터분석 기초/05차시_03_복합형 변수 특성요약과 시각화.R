######################################################################
# [05차시: 개별변수 요약과 집계] 03. 복합형 변수 특성요약과 시각화
######################################################################
 
# --------------------------------------------------
# 데이터셋 로딩
# --------------------------------------------------

# utils::read.csv()함수로 다중응답 데이터셋 로딩
mra <- read.csv(file = 'tour_mra.csv', 
                header = TRUE, sep = ',',
                stringsAsFactors = FALSE, 
                strip.white = TRUE,
                na.strings = c('.', '?', 'NA'))

# 범주형 데이터셋 간단 조회
head(mra)
tail(mra)

# 내부구조 조회
str(mra)

# 기본 기술통계량 파악
summary(mra)

# --------------------------------------------------
# 여행동기 변수컬럼 별도 데이터셋으로 추출
# --------------------------------------------------

# 여행동기 변수컬럼 서브데이터셋 추출
# --------------------------------------------------
# 대괄호연산자 이용 여행동기 변수컬럼 부분인덱싱 추출
mv <- mra[c('mov1', 'mov2', 'mov3')]
str(mv)

# base::subset()함수 이용 여행동기 변수컬럼 부분인덱싱 추출
mov <- subset(mra, select = c(mov1, mov2, mov3))
str(mov)

# 두 가지 방식 추출객체 동일성 비교
identical(mv, mov)


# 여행동기 변수컬럼 기술통계량 분석
# --------------------------------------------------
# 여행동기 세부변수별 기술통계량
summary(mov)

# install.packages('summarytools')
library(summarytools)
summarytools::descr(mov)
dfSummary(mov)


# 여행동기 변수컬럼 빈도수 분석
# --------------------------------------------------
# 여행동기 세부변수별 빈도수 카운팅
table(mov) 
# - 3가지 여행동기 세부변수 중에서 2개씩 짝을지어 빈도수 카운팅이 됨
table(mov$mov1); table(mov$mov2); table(mov$mov3)
# - 같은주제에 속한 개별변수들을 하나씩 빈도분석을 실시하는 것은 비효율적임

# 데이터순환함수를 이용한 여행동기 각 변수별 빈도수 카운팅
sapply(mov, table)

# 여행동기 통합빈도수 산정
mov_freq <- table(as.matrix(mov)) # 여행동기 세부변수를 하나의 행렬데이터로 통합해서 빈도수를 카운팅해줌
mov_freq

# 여행동기 통합 빈도수 테이블에 레이블 반영
names(mov_freq) <-  c('자연풍경', '쇼핑', '역사문화유적', '음식탐방', 
                      '휴양시설', '놀이시설', '문화트렌드', '사계절', 
                      '적정경비', '거리', '안전치안', '시설교통', 
                      '한류체험', '치료', '이미용', '여가시간', '기타')

# 레이블반영된 여행동기 통합빈도수 확인
mov_freq

# 빈도수 기준 내림차순 정열
sort(mov_freq, decreasing = TRUE)

# 여행동기 통합빈도수 데이터프레임객체로 변환
mov_freq_df <- as.data.frame(mov_freq)
mov_freq_df


# 여행동기 변수컬럼 비율 분석
# --------------------------------------------------
# 여행동기 통합빈도수 비율내용 확인
mov_prop <- round(prop.table(mov_freq), 3)
mov_prop 

# 비율 기준 내림차순 정열
sort(mov_prop, decreasing = TRUE) 

# 여행동기 비율내용 데이터프레임객체로 변환
mov_prop_df <- as.data.frame(mov_prop)
mov_prop_df


# 여행동기 변수컬럼 백분율 분석
# --------------------------------------------------
# 여행동기 통합빈도수 백분율내용 확인
mov_pect <- mov_prop * 100
mov_pect

# 백분율 기준 내림차순 정열
sort(mov_pect, decreasing = TRUE)

# 여행동기 백분율내용 데이터프레임객체로 변환
mov_pect_df <- as.data.frame(mov_pect)
mov_pect_df


# 여행동기 다중응답분석 결과테이블 생성
# --------------------------------------------------

# 여행동기 통합빈도수 계산내용
mov_freq_df
# 여행동기 통합빈도수를 비율로 변환한 내용
mov_prop_df
# 여행동기 통합빈도수를 백분율로 변환한 내용
mov_pect_df


# 여행동기 관련 다중응답분석 결과를 하나의 데이터프레임으로 연결
mov_result <- data.frame(motivation = mov_freq_df$Var1, 
                         frequency = mov_freq_df$Freq,
                         percentage = mov_pect_df$Freq)

mov_result

# 여행동기 다중응답분석결과를 빈도수를 기준으로 정렬
library(dplyr)
mov_result_sort <- arrange(mov_result, desc(frequency))
mov_result_sort


# 여행동기 다중응답 분석 시각화
# --------------------------------------------------
# barplot() 함수를 이용한 범주형 데이터 그래프

# 기본 막대그래프 생성
barplot(mov_freq,
        main = "외래관광객 한국방문 동기요인 분포",
        xlab = "동기요인", ylab = "응답자수")

# 색상파레트에서 색상스펙트럼 선정
# install.packages('RColorBrewer')
library(RColorBrewer)

display.brewer.all()
mycol = brewer.pal(9, "Set2") 
mycol

# 막대그래프 업그레이드
p <- barplot(sort(mov_freq, decreasing = TRUE),
             main = "외래관광객 한국방문 동기요인 분포",
             xlab = "동기요인", ylab = "응답자수",
             col = mycol, xaxt = 'n')

# x축 눈금레이블 텍스트 출력조정
text(pos = 1, x = p, y = -100, srt = 45, cex = 0.7, xpd = TRUE,
     labels = names(sort(mov_freq, decreasing = TRUE)))


### End of Source ####################################################

