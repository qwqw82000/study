######################################################################
# [07차시: 데이터 가공] 02. 요약변수 만들기
######################################################################

 
# --------------------------------------------------
# 요인화를 통한 요약변수 만들기
# --------------------------------------------------
# - 여러 개의 변수들을 하나의 대표변수로 요약함


# 세부만족도(ds: detail_satisfaction) 데이터셋 로딩
# --------------------------------------------------

# utils::read.csv()함수로 로딩
ds <- read.csv(file = 'tour_ds.csv', 
               header = TRUE, sep = ',',
               stringsAsFactors = FALSE, 
               strip.white = TRUE,
               na.strings = c('.', '?', 'NA'))

head(ds)

# id 변수 제외
ds <- ds[-1]


# 세부만족도 변수들 기본탐색
# --------------------------------------------------
str(ds)
psych::describe(ds)
Hmisc::describe(ds)


# 세부만족도 변수들을 관광상품과 관광인프라 요인(집합변수)으로 요약집계
# --------------------------------------------------
# 관광상품(tour_prd)		
# - 숙박(lodge), 음식(food), 쇼핑물품(goods), 볼거리(site), 여행경비(cost)

# 관광인프라(tour_infra)
# - 출입국절차(process), 대중교통(trans), 관광안내(guide), 언어소통(comm), 치안(safety)

# 관광상품 관련 변수컬럼목록 설정
tour_prd <- c('lodge', 'food', 'goods', 'site', 'cost')
tour_prd

# 관광인프라 관련 변수컬럼목록 설정
tour_infra <- setdiff(names(ds), tour_prd)
tour_infra

# 관광상품과 관광인프라별 평균만족도 계산
ds$tour_prd_sat <- apply(ds[tour_prd], 1, mean)
ds$tour_infra_sat <- apply(ds[tour_infra], 1, mean)

head(ds)


# 세부만족도 변수들을 전체적으로 요약집계하여 전반적 평균만족도 변수로 생성
# --------------------------------------------------
ds$tour_overal_sat <- apply(ds, 1, mean)

head(ds)


# 세부만족도 변수들을 활용해 요약집계한 변수들 기술통계분석
tour_satis <- ds[c('tour_prd_sat', 'tour_infra_sat', 'tour_overal_sat')]
head(tour_satis)

psych::describe(tour_satis)
Hmisc::describe(tour_satis)

# 시각화를 통한 비교
boxplot(tour_satis)


### End of Source ####################################################

