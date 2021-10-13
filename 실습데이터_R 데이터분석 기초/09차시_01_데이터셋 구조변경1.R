######################################################################
# [09차시: 데이터 구조변형] 01. 데이터셋 구조변경1
######################################################################

 
# --------------------------------------------------
# 데이터셋 준비
# --------------------------------------------------

# 구조변경용 데이터셋 준비
# install.packages("tibble")
library(tibble)

raw <- tibble(cusid = c("c01", "c02", "c03", "c04", "c05"),
              gender = c(2, 1, 1, 2, 1), 
              wk1 = c(4, NA, 15, 3, 6),
              wk2 = c(NA, NA, 8, 15, 22),
              wk3 = c(11, 2, 14, 23, NA),
              wk4 = c(19, 25, 28, 13, 6),
              wk5 = c(22, 18, NA, 19, 14),
              wk6 = c(5, 13, NA, 7, NA),
              wk7 = c(12, 8, NA, 10, 30))
raw
# 전반적인 raw 데이터 스캐닝
str(raw)
summary(raw)


# --------------------------------------------------
# 와이드형에서 롱포맷으로 변경
# --------------------------------------------------

# tidyr::gather() 함수이용 와이드형에서 롱형으로 데이터구조 변환
library(tidyr)
raw_long <- gather(data = raw, 
                   key = week, value = result, 
                   wk1, wk2, wk3, wk4, wk5, wk6, wk7)

# 와이드형에서 롱포맷으로 재구조화된 데이터셋 확인
raw_long
str(raw_long) 

# cusid로 소팅
library(magrittr); library(dplyr)
raw_long %<>% arrange(cusid)
raw_long

# 와이드포맷에서 롱포맷으로 변환된 변수컬럼 레이블 변경
library(doBy)
raw_long$week %<>% recodeVar(src = c("wk1", "wk2", "wk3", "wk4", 
                                     "wk5", "wk6", "wk7"),
                             tgt = c(1, 2, 3, 4, 5, 6, 7))
raw_long
str(raw_long)


# --------------------------------------------------
# 롱형에서 와이드형으로 변경
# --------------------------------------------------

# 현재의 롱형 데이터셋 재확인 
raw_long

# tidyr::spread() 함수이용 롱형에서 와이드형으로 데이터구조 변환
library(tidyr)

raw_wide <- spread(data = raw_long,
                   key = week, value = result)

# 롱포맷에서 와이드형으로 재구조화된 데이터셋 확인
raw_wide
str(raw_wide)

# 와이드형으로 변환된 데이터셋의 변수컬럼명 지정
raw_wide %<>% dplyr::rename(wk1 = "1", wk2 = "2", wk3 = "3", wk4 = "4",
                            wk5 = "5", wk6 = "6", wk7 = "7")

raw_wide
str(raw_wide)

# 데이터셋 일치성 점검
identical(raw, raw_wide)


# --------------------------------------------------
# 롱포맷 데이터셋을 활용한 요약집계실시
# --------------------------------------------------

# 원본 와이드포맷 데이터셋 내용조회
raw 

# 롱포맷 데이터셋 내용조회
raw_long 

# 롱포맷 데이터셋을 이용한 요약집계 실시
raw_final <- raw_long %>% group_by(cusid) %>%
  dplyr::summarize(result_n = sum(!is.na(result)),
                   result_sum = sum(result, na.rm = TRUE),
                   result_avg = mean(result, na.rm = TRUE),
                   result_median = median(result, na.rm = TRUE),
                   result_sd = sd(result, na.rm = TRUE),
                   result_min = min(result, na.rm = TRUE),
                   result_max = max(result, na.rm = TRUE))

# 요약집계 내용 정렬
raw_final
raw_final %>% 
  arrange(desc(result_sum), # 7주간 실적총합을 1순위로 내림차순 정렬 
          desc(result_avg), # 주당 거래실적 평균값을 2순위로 내림차순 정렬 
          result_sd) # 주당 거래실적 표준편차를 3순위로 오름차순 정렬


### End of Source ####################################################

