######################################################################
# [09차시: 데이터 구조변형] 01. 데이터셋 구조변경2
######################################################################


# ------------------------------ --------------------
# 데이터셋 준비
# --------------------------------------------------

# trans 데이터 준비
# install.packages("tibble")
library(tibble)

## 가상 데이터 생성
# - customID: 고객번호
# - buyDate: 구매일
# - pruduct: 상품1 구매갯수
# - pruduct2: 상품2 구매갯수

trans <- tibble(customID = c("KD257", "KD257", "KD257", 
                             "CT303", "CT303", "AQ115", "AQ115"),
                buyDate = c(170720, 170815, 170720, 
                            170815, 170720, 170720, 170815),
                product1 = c(15, 23, 9, 42, 12, 31, 51),
                product2 = c(16, 7, 21, 34, 9, 65, 27))

# trans 데이터셋에서 요약집계분석의 기준변수로 사용
trans
str(trans)
summary(trans)


# --------------------------------------------------
# 와이드형에서 롱포맷으로 변경
# --------------------------------------------------

# reshape::melt() 함수이용 와이드형에서 롱형으로 데이터구조 변환
# install.packages("reshape2")
library(reshape2)

trans.long <- melt(data = trans, id.vars = c("customID", "buyDate"),
                   measure.vars = c("product1", "product2"),
                   variable.name = "pd", value.name = "output") 

# 와이드형에서 롱포맷으로 재구조화된 데이터셋 확인
trans.long
str(trans.long) 

# reshape::melt()함수 사용시 기본키(id.vars=) 설정이 중요
melt(trans, id.vars = c("customID"))
melt(trans, id.vars = c("buyDate"))

# cusid로 소팅
library(magrittr); library(dplyr)

trans.long %<>% arrange(customID)
# cusid로 소팅
trans.long


# --------------------------------------------------
# 롱형에서 와이드형으로 변경
# --------------------------------------------------

# 현재의 롱형 데이터셋 재확인
trans.long

# reshape2::dcast() 함수이용 롱형에서 와이드형으로 데이터구조 변환
# install.packages("reshape2")
library(reshape2)

# 롱포맷에서 와이드로 재구조화 + 집계함수가 없는 방식
dcast(data = trans.long, 
      formula = customID ~ pd, 
      value.var = "output") 

# 롱포맷에서 와이드로 재구조화 + 집계함수가 설정방식
dcast(data = trans.long, 
      formula = customID ~ pd, 
      value.var = "output",
      fun.aggregate = mean) 

# - 집계함수를 설정하면 데이터 재구조화와 그에 따른 집계가 동시에 작동함
# - 틸드(~)기호를 중심으로 
#   왼쪽(집계기준변수, 행방향) ~ 오른쪽(집계결과변수: 열방향)
# - 그리고 집계함수 사용으로 구체적인 집계기준에 따른 집계결과가 나옴


# --------------------------------------------------
# 데이터셋을 활용한 요약집계실시
# --------------------------------------------------


# reshape2::dcast() 함수 이용
# --------------------------------------------------
# 다양한 포뮬러(관계식) 설정을 통한 데이터재구조화 + 집계
# - data =, formaula = , value.var = , fun.aggregate = 옵션 생략가능

dcast(trans.long, customID ~ pd)
dcast(trans.long, customID ~ pd, mean)

dcast(trans.long, customID ~ buyDate)
dcast(trans.long, customID ~ buyDate, mean)

dcast(trans.long, buyDate ~ pd)
dcast(trans.long, buyDate ~ pd, mean)

dcast(trans.long, buyDate ~ customID)
dcast(trans.long, buyDate ~ customID, mean)

dcast(trans.long, customID + buyDate ~ pd)
dcast(trans.long, customID + buyDate ~ pd, mean)

dcast(trans.long, buyDate + customID ~ pd)
dcast(trans.long, buyDate + customID ~ pd, mean)
dcast(trans.long, customID ~ buyDate + pd, mean)


# dplyr::group_by(), summarize() 함수 이용
# --------------------------------------------------
# 다양한 관점의 집계
# - 다양한 집계기준에 따라 다양한 집계결과변수의 특성비교를
#   다양한 집계연산함수를 동시에 적용할 수 있음
## 개별 고객별 실적비교
trans.long %>% group_by(customID) %>%
  dplyr::summarize(output_n = sum(!is.na(output)),
                   output_sum = sum(output, na.rm = TRUE),
                   output_avg = mean(output, na.rm = TRUE),
                   outputmedian = median(output, na.rm = TRUE),
                   output_sd = sd(output, na.rm = TRUE),
                   output_min = min(output, na.rm = TRUE),
                   output_max = max(output, na.rm = TRUE))
## 개별고객 + 일자별 실적비교
trans.long %>% group_by(customID, buyDate) %>%
  dplyr::summarize(output_n = sum(!is.na(output)),
                   output_sum = sum(output, na.rm = TRUE),
                   output_avg = mean(output, na.rm = TRUE),
                   outputmedian = median(output, na.rm = TRUE),
                   output_sd = sd(output, na.rm = TRUE),
                   output_min = min(output, na.rm = TRUE),
                   output_max = max(output, na.rm = TRUE))


### End of Source ####################################################

