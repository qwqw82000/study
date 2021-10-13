######################################################################
# [10차시: 데이터셋 결합과 분리] 01. 데이터셋 바인딩
######################################################################
 
 
# --------------------------------------------------
# 행(row) 기준 바인딩용 데이터셋 준비
# --------------------------------------------------

# rbind(), bind_rows() 함수용 예제데이터셋 생성
# --------------------------------------------------
A <- data.frame(id = c("c01","c02","c03","c04"), 
                last = c("Kim", "Lee", "Choi", "Park"),
                gender = c("F", "M", "F", "M"), 
                stringsAsFactors = FALSE)

B <- data.frame(id = c("c05", "c06", "c07"), 
                gender = c("F", "F", "M"),
                last = c("Bae", "Kim", "Lim"),
                stringsAsFactors = FALSE)

C <- data.frame(id = c("c08", "c09"), 
                last = c("Lee", "Park"), 
                stringsAsFactors = FALSE)

D <- data.frame(id = c("c10", "c11", "c12"), 
                last = c("Bang", "Kang", "Rim"),
                gender = c("M", "F", "M"),
                job = c(4, 3, 1), 
                stringsAsFactors = FALSE)

E <- data.frame(id = c("c13", "c14", "c15"), 
                first = c("Sangmin", "Hyori", "Daniel"),
                gender = c("M", "M", "F"), 
                stringsAsFactors = FALSE)


# 예제데이터셋 내용 조회
# --------------------------------------------------
A # id, last, gender, n = 4
B # id, gender, last, n = 3
C # id, last, n = 2
D # id, last, gender, job, n = 3
E # id, first, gender, n = 3


# --------------------------------------------------
# 행(row) 기준 바인딩
# --------------------------------------------------

# base::rbind()와 dplyr::bind_rows()를 이용함
# --------------------------------------------------
# 두 개 데이터셋 간에 행을 기준으로 위·아래로 결합하는 행기준 바인딩 작업
library(dplyr)

rbind(A, B)  
bind_rows(A, B)
identical(rbind(A, B), bind_rows(A, B))
# - id, last, gender ==정상결합== id, gender, last

rbind(B, A)
bind_rows(B, A)
identical(rbind(B, A), bind_rows(B, A))
# - id, gender, last ==정상결합== id, last, gender
 
rbind(A, C)  # 에러발생이 정상, 에러원인을 찾으시요.
bind_rows(A, C)
# - id, last, gender <=에러발생=> id, last

rbind(A, D)  # 에러발생이 정상, 에러원인을 찾으시요.
bind_rows(A, D) 
# - id, last, gender <=에러발생=> id, last, gender, job

rbind(A, E)  # 에러발생이 정상, 에러원인을 찾으시요.
bind_rows(A, E)
# - id, last, gender <=에러발생=> id, first, gender


# --------------------------------------------------
# 열(column) 기준 바인딩용 데이터셋 준비
# --------------------------------------------------

# cbind(), bind_cols() 함수용 예제데이터셋 생성
# --------------------------------------------------
A <- data.frame(id = c("c01","c02","c03","c04"), 
                last = c("Kim", "Lee", "Choi", "Park"),
                gender = c("F", "M", "F", "M"), 
                stringsAsFactors = FALSE)

P <- data.frame(age = c(20, 25, 19, 40), 
                income = c(2500, 2700, 0, 7000), 
                stringsAsFactors = FALSE)

Q <- data.frame(age = c(20, 25, 19, 40, 32, 39, 28), 
                income = c(2500, 2700, 0, 7000, 3400, 3600, 2900), 
                stringsAsFactors = FALSE)

R <- data.frame(id=c("c01","c02","c03","c04"),
                age = c(20, 25, 19, 40), 
                income = c(2500, 2700, 0, 7000), 
                stringsAsFactors = FALSE)

S <- data.frame(id=c("c03","c04","c07","c08"),
                age = c(19, 40, 29, 30), 
                income = c(1500, 3400, 3020, 4500), 
                stringsAsFactors = FALSE)


# 예제데이터셋 내용 조회
# --------------------------------------------------
A # id, last, gender, n = 4
P # age, income, n=4
Q # age, income, n=7
R # age, income, gender, n=4
S # id, age, income, n=4


# --------------------------------------------------
# 열(column) 기준 바인딩
# --------------------------------------------------

# base::cbind()와 dplyr::bind_cols()를 이용함
# --------------------------------------------------
# 두 개 데이터셋 간에 열(column)을 기준으로 좌·우로 결합하는 열기준 바인딩 작업
library(dplyr)

cbind(A, P) 
bind_cols(A, P)
identical(cbind(A, P), bind_cols(A, P))
# - 물리적 결합성공, 그러나 논리적 에러! 
# - P데이터셋에 pk변수가 없어 연결된 레코드가 
#   실제 A데이터셋의 id번호고객인지 불확실함

cbind(P, A) 
bind_cols(P, A)
identical(cbind(P, A), bind_cols(P, A))
# - 상동

cbind(A, Q) 
bind_cols(A, Q) 
# - 레코드 갯수가 달라서 
#   물리적으로 우선 에러발생

cbind(A, R)
bind_cols(A, R) 
# - 물리적 & 논리적 정상결합, 양쪽 데이터셋에 
#   pk(primary key)변수가 있고 
#   실제 고객 id번호도 일치함

cbind(A, S)
bind_cols(A, S) 
# - 물리적 결합성공, 그러나 논리적 에러!
# - 양쪽 데이터셋에 pk변수가 있지만 
#   실제 고객 id번호가 일치하지 않음 


# --------------------------------------------------
# 집합연산용 데이터셋 준비
# --------------------------------------------------

# tibble객체형식으로 집합연산용 데이터셋 준비
library(tibble)

M <- tibble(x1 = c('Ace', 'Best', 'Cross'), 
            x2 = c(1, 2, 3))

N <- tibble(x1 = c('Best', 'Cross', 'Dream', 'Energy'), 
            x2 = c(2, 3, 4, 5))

O <- tibble(x2 = c(1, 3, 7, 8),
            x3 = c('Ace', 'Cross', 'Bird', 'Giant'))

# 예제데이터셋 내용 조회
# --------------------------------------------------
M # x1, x1, n = 3
N # x1, x2, n = 4
O # x2, x3, n = 3


# 합집합(union set) 데이터셋 만들기
# --------------------------------------------------
library(dplyr)

union(M, N)
dplyr::union(M, N)

union(N, M)
dplyr::union(N, M)

union(M, O)  # 에러 발생이 정상, 에러 원인을 찾으시요?
union(N, O)  # 에러 발생이 정상, 에러 원인을 찾으시요?


# 교집합(intersect set) 데이터셋 만들기
# --------------------------------------------------
intersect(M, N)
dplyr::intersect(M, N)

intersect(N, M)
dplyr::intersect(N, M)

intersect(M, O) #에러발생이 정상, 에러원인을 찾으시요.
intersect(N, O) #에러발생이 정상, 에러원인을 찾으시요.


# 차집합(difference set) 데이터셋 만들기
# --------------------------------------------------
setdiff(M, N)
dplyr::setdiff(M, N)

setdiff(N, M)
dplyr::setdiff(N, M)

setdiff(M, O) #에러발생이 정상, 에러원인을 찾으시요.
setdiff(N, O) #에러발생이 정상, 에러원인을 찾으시요.


### End of Source ####################################################

