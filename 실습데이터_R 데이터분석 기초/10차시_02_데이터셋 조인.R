######################################################################
# [10차시: 데이터셋 결합과 분리] 02. 데이터셋 조인
######################################################################

 
# --------------------------------------------------
# 내부조인(inner join)을 이용한 데이터셋간 정보추출 
# --------------------------------------------------

# 내부조인 대상 2개 데이터셋 생성
# --------------------------------------------------
A <- data.frame(id = c("c01","c02","c03","c04"), 
                last = c("Kim", "Lee", "Choi", "Park"),
                gender = c("F", "M", "F", "M"), 
                stringsAsFactors = FALSE)

S <- data.frame(id = c("c03","c04","c07","c08"),
                age = c(19, 40, 29, 30), 
                income = c(1500, 3400, 3020, 4500), 
                stringsAsFactors = FALSE)


# 예제데이터셋 내용 조회
# --------------------------------------------------
A # id, last, gender, n = 4
S # id, age, income, n = 4


# 내부조인 실시
# --------------------------------------------------
# 기본 base::merge()함수 이용
merge(x=A,  y=S)
merge(x=A,  y=S,  by='id')

# dplyr::join 계열함수 이용
inner_join(x=A,  y=S)
inner_join(x=A,  y=S,  by='id')

# magrittr:: %>% # 파이프 연산자이용
A %>% inner_join(S)
A %>% inner_join(S,  by='id')


# --------------------------------------------------
# 외부조인(outer join)을 이용한 데이터셋간 정보추출 
# --------------------------------------------------

# 외부조인 대상 2개 데이터셋 생성
# --------------------------------------------------
A <- data.frame(id = c("c01","c02","c03","c04"), 
                last = c("Kim", "Lee", "Choi", "Park"),
                gender = c("F", "M", "F", "M"), 
                stringsAsFactors = FALSE)

S <- data.frame(id = c("c03","c04","c07","c08"),
                age = c(19, 40, 29, 30), 
                income = c(1500, 3400, 3020, 4500), 
                stringsAsFactors = FALSE)


# 예제데이터셋 내용 조회
# --------------------------------------------------
A # id, last, gender, n = 4
S # id, age, income, n = 4


# 외부조인 실시
# --------------------------------------------------
# 기본 base::merge()함수 이용
merge(x=A,  y=S, all = TRUE)
merge(x=A,  y=S,  by='id', all = TRUE)

# dplyr::join 계열함수 이용
full_join(x=A,  y=S)
full_join(x=A,  y=S,  by='id')

# magrittr:: %>% # 파이프 연산자이용
A %>% full_join(S)
A %>% full_join(S,  by='id')


# --------------------------------------------------
# 왼쪽외부조인(left outer join)을 이용한 데이터셋간 정보추출 
# --------------------------------------------------

# 왼쪽외부조인 대상 2개 데이터셋 생성
# --------------------------------------------------
A <- data.frame(id = c("c01","c02","c03","c04"), 
                last = c("Kim", "Lee", "Choi", "Park"),
                gender = c("F", "M", "F", "M"), 
                stringsAsFactors = FALSE)

S <- data.frame(id = c("c03","c04","c07","c08"),
                age = c(19, 40, 29, 30), 
                income = c(1500, 3400, 3020, 4500), 
                stringsAsFactors = FALSE)


# 예제데이터셋 내용 조회
# --------------------------------------------------
A # id, last, gender, n = 4
S # id, age, income, n = 4


# 왼쪽외부조인 실시
# --------------------------------------------------
# 기본 base::merge()함수 이용
merge(x=A,  y=S, all.x = TRUE)
merge(x=A,  y=S,  by='id', all.x = TRUE)

# dplyr::join 계열함수 이용
left_join(x=A, y=S)
left_join(x=A, y=S, by='id')

# magrittr:: %>% # 파이프 연산자이용
A %>% left_join(S)
A %>% left_join(S, by='id')


# --------------------------------------------------
# 오른쪽외부조인(right outer join)을 이용한 데이터셋간 정보추출 
# --------------------------------------------------

# 오른쪽외부조인 대상 2개 데이터셋 생성
# --------------------------------------------------
A <- data.frame(id = c("c01","c02","c03","c04"), 
                last = c("Kim", "Lee", "Choi", "Park"),
                gender = c("F", "M", "F", "M"), 
                stringsAsFactors = FALSE)

S <- data.frame(id = c("c03","c04","c07","c08"),
                age = c(19, 40, 29, 30), 
                income = c(1500, 3400, 3020, 4500), 
                stringsAsFactors = FALSE)


# 예제데이터셋 내용 조회
# --------------------------------------------------
A # id, last, gender, n = 4
S # id, age, income, n = 4


# 오른쪽외부조인 실시
# --------------------------------------------------
# 기본 base::merge()함수 이용
merge(x=A,  y=S, all.y = TRUE)
merge(x=A,  y=S,  by='id', all.y = TRUE)

# dplyr::join 계열함수 이용
right_join(x=A,  y=S)
right_join(x=A,  y=S,  by='id')

# magrittr:: %>% # 파이프 연산자이용
A %>% right_join(S)
A %>% right_join(S,  by='id')


# --------------------------------------------------
# 세미조인(semi join)을 이용한 데이터셋간 정보추출 
# --------------------------------------------------

# 세미조인 대상 2개 데이터셋 생성
# --------------------------------------------------
A <- data.frame(id = c("c01","c02","c03","c04"), 
                last = c("Kim", "Lee", "Choi", "Park"),
                gender = c("F", "M", "F", "M"), 
                stringsAsFactors = FALSE)

S <- data.frame(id = c("c03","c04","c07","c08"),
                age = c(19, 40, 29, 30), 
                income = c(1500, 3400, 3020, 4500), 
                stringsAsFactors = FALSE)


# 예제데이터셋 내용 조회
# --------------------------------------------------
A # id, last, gender, n = 4
S # id, age, income, n = 4


# 세미조인 실시
# --------------------------------------------------
# dplyr::join 계열함수 이용
semi_join(x=A,  y=S)
semi_join(x=A,  y=S,  by='id')

# magrittr:: %>% # 파이프 연산자이용
A %>% semi_join(S)
A %>% semi_join(S,  by='id')



# --------------------------------------------------
# 안티조인(anti join)을 이용한 데이터셋간 정보추출 
# --------------------------------------------------

# 안티조인 대상 2개 데이터셋 생성
# --------------------------------------------------
A <- data.frame(id = c("c01","c02","c03","c04"), 
                last = c("Kim", "Lee", "Choi", "Park"),
                gender = c("F", "M", "F", "M"), 
                stringsAsFactors = FALSE)

S <- data.frame(id = c("c03","c04","c07","c08"),
                age = c(19, 40, 29, 30), 
                income = c(1500, 3400, 3020, 4500), 
                stringsAsFactors = FALSE)


# 예제데이터셋 내용 조회
# --------------------------------------------------
A # id, last, gender, n = 4
S # id, age, income, n = 4


# 안티조인 실시
# --------------------------------------------------
# dplyr::join 계열함수 이용
anti_join(x=A,  y=S)
anti_join(x=A,  y=S,  by='id')

# magrittr:: %>% # 파이프 연산자이용
A %>% anti_join(S)
A %>% anti_join(S,  by='id')


### End of Source ####################################################

