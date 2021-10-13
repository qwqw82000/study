######################################################################
# [10차시: 데이터셋 결합과 분리] 03. 데이터셋 분할
######################################################################
 
# --------------------------------------------------
# 데이터셋 분리용 가상데이터 생성
# --------------------------------------------------

# 샘플재현성을 위한 랜덤시드넘버 생성
set.seed(123)

# 필요한 샘플갯수 설정
n <- 12


# 무작위 샘플링 방법으로 각 변수컬럼 데이터 생성
# --------------------------------------------------
gender <- sample(c("f", "m"), n, replace=TRUE) %>% print
group <- sample(rep(c("EU", "AP", "SEA"), 4), n, 
                replace = FALSE) %>% print
age <- sample(18:35, n, replace = TRUE) %>% print
expense <- round(rnorm(n, mean = 1000, sd = 15)) %>% print
overal <- round(runif(n, min = 0, max = 5)) %>% print


# 데이터프레임으로 구조화
# --------------------------------------------------
df <- data.frame(id = 1:n, gender, group, age, expense, overal)
df

# 데이터프레임 탐색
str(df)
summary(df)


# --------------------------------------------------
# 성별(gender) 변수기준 데이터셋 분할: 2개 범주항목
# --------------------------------------------------

# base::split() 함수이용 데이터셋 분할
df_gender <- split(df, df$gender)
df_gender
str(df_gender)

# 분할된 내용 중 여성 데이터셋 별도 추출
df_female <- df_gender[[1]]
df_female
str(df_female)
table(df_female$gender)

# 분할된 내용 중 남성 데이터셋 별도 추출
df_male <- df_gender[[2]]
df_male
str(df_male)
table(df_male$gender)


# --------------------------------------------------
# 성별(gender) 변수기준 분할된 데이터셋 재결합
# --------------------------------------------------

# base::unsplit() 함수이용
df_re <- unsplit(df_gender, df$gender)
df_re
table(df_re$gender)

# base::rbind() 함수이용
df_binding <- rbind(df_male, df_female)
df_binding
table(df_binding$gender)


# --------------------------------------------------
# 성별(gender)와 지역그룹(group) 변수기준 데이터셋 분할 
# --------------------------------------------------
# gender: m, f
# group: AP(아시아퍼시픽), EU(유럽), SEA(동남아시아)

# base::split() 함수이용 데이터셋 분할
df_gd_gr <- split(df, list(df$gender, df$group))
df_gd_gr
str(df_gd_gr)

# 분할된 내용 중 여성이면서 유럽그룹에 속한 데이터셋 별도 추출
df_f_EU <- df_gd_gr[['f.EU']]
df_f_EU
str(df_f_EU)
table(df_f_EU$group)

# 분할된 내용 중 남성이면서 동남아시아그룹에 속한 데이터셋 별도 추출
df_m_SEA <- df_gd_gr[[6]]
df_m_SEA
str(df_m_SEA)
table(df_m_SEA$group)


# --------------------------------------------------
# 성별(gender)와 지역그룹(group) 변수기준 분할된 데이터셋 재결합
# --------------------------------------------------

# base::unsplit() 함수이용
df_origin <- unsplit(df_gd_gr, list(df$gender, df$group))
df_origin


### End of Source ####################################################


