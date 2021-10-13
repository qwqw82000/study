######################################################################
# [03차시: R자료형과 데이터셋로딩] 02. R 복합자료형 익히기
######################################################################


# --------------------------------------------------
# 데이터객체: 데이터프레임(data.frame)
# --------------------------------------------------

# 벡터변수 변수정의
name <- c("홍길동", "Jessica", "장그래", "제인", NA, "최강타")
gender <- c(1, 2, NA, 2, 1, 1) # 1: 남자, 2: 여자
job <- c("학생", NA, "직장인", "주부", "직장인", "직장인")
age <- c(19, 55, 23, 35, 45, NA) # 연령
grade <- c("A", "C", "B", NA, NA, "A") # 회원등급
total <- c(NA, 18.5, 23.7, 27.0, 13.5, 47.2) # 총구매금액(만원)
survey <- c(3, 4, 2, 5, 5, 3) # 온라인쇼핑 만족도
result <- c(TRUE, T, FALSE, TRUE, NA, F) # 이벤트반응

# 벡터변수를 데이터프레임으로 결합
customer <- data.frame(name, gender, job, age, grade, survey, total, result)
class(customer)

customer
View(customer)

# 데이터프레임 변수컬럼명 확인
colnames(customer); names(customer)

# 데이터프레임 레코드명 확인
rownames(customer); row.names(customer)


# --------------------------------------------------
# 데이터프레임 인덱싱
# --------------------------------------------------

# 달러연산자 $기호에 변수컬럼명 조회
customer$gender
customer$age
# - 인덱싱결과는 벡터

# 대괄호연산자 [ ]기호에 변수컬럼명 조회
customer["gender"]
customer["age"]
# - 인덱싱결과는 데이터프레임 서브셋

# 대괄호연산자 [ ]기호에 변수컬럼 위치 조회
customer[2]
customer[4]
# - 인덱싱결과는 데이터프레임 서브셋

# 대괄호 [ ]기호에 복수 변수컬럼명 조회
customer[c("gender", "age")]
customer[c("gender", "job", "age")]


# 대괄호 [ ]기호에 복수 변수컬럼 위치 조회
customer[c(2, 4)]
customer[c(2, 3, 4)]
customer[c(2:4)]
customer[(2:4)]
customer[2:4]

# 마이너스 인덱싱방법을 통해 불필요 변수컬럼 제외
customer[-2]
customer[-c(2, 4)]
customer[-2:-4]
customer[-c(2:4)]


# --------------------------------------------------
# 데이터프레임 구조파악
# --------------------------------------------------

# 데이터프레임 구조(structure) 확인
# --------------------------------------------------
str(customer)

# 데이터프레임 생성시 팩터변수 생성옵션 사용유무
customer <- data.frame(name, gender, job, age, 
                       grade, survey, total, result,
                       stringsAsFactors = FALSE)
str(customer)


# 특정 변수컬럼을 팩터형 데이터로 만들어 주는 방법
# --------------------------------------------------
# 현재 이름(name)과 성별(gender) 변수상태
customer$name
customer$gender

# factor()함수나 as.factor()함수를 사용
customer$name <- factor(customer$name)
customer$name

customer$gender <- factor(customer$gender)
customer$gender

str(customer)

# 팩터형 변수컬럼을 다시 일반형 데이터로 만들어 주는 방법
customer$name <- as.character(customer$name)
customer$name

customer$gender <- as.integer(customer$gender)
customer$gender


# --------------------------------------------------
# 데이터객체: 리스트(list)
# --------------------------------------------------

# 지금까지 생성한 다양한 R 데이터객체 생성코드 재확인

# 스칼라 데이터 객체의 변수정의
a <- 365

# 벡터 데이터 객체의 변수정의
h3 <- c(NA, "JK Tim", "John", "장그래", NA)

# 행렬 데이터 객체의 변수정의
m1 <- c(10, -2, 5:7)
m2 <- c(2, 12:14, -5)
mxr <- rbind(m1, m2) # 두 벡터를 위아래로 결합
rownames(mxr) <- c("아이폰", "갤럭시") # 행이름 변경
colnames(mxr) <- c("1월", "2월", "3월", "4월", "5월") # 열이름
mxr

# 배열 데이터 객체의 변수정의
raw1 <- c(8, 10, 7, 16, NA, -1, 8, 4)
raw2 <- c(4, NA, 8, 7, -2, 17, -3, 10)
raw3 <- c(23, -9, NA, NA, 11, 9, 4, 12)
out <- array(data = c(raw1, raw2, raw3), dim = c(4, 2, 3))
mylayer <- c("US", "EU", "AP") # 배열 행이름
myrow <- c("1분기", "2분기", "3분기", "4분기") # 배열 열이름
mycol <- c("아이폰", "갤럭시") # 배열 레이어이름
dimnames(out) <- list(myrow, mycol, mylayer) # 배열이름 변경
out

# 데이터프레임 객체의 변수정의
name <- c("홍길동", "Jessica", "장그래", "제인", NA, "최강타")
gender <- c(1, 2, NA, 2, 1, 1) # 1: 남자, 2: 여자
job <- c("학생", NA, "직장인", "주부", "직장인", "직장인")
age <- c(19, 55, 23, 35, 45, NA) # 연령
grade <- c("A", "C", "B", NA, NA, "A") # 회원등급
total <- c(NA, 18.5, 23.7, 27.0, 13.5, 47.2) # 총구매금액(만원)
survey <- c(3, 4, 2, 5, 5, 3) # 온라인쇼핑 만족도
result <- c(TRUE, T, FALSE, TRUE, NA, F) # 이벤트반응
customer <- data.frame(name, gender, job, age, grade, survey, total, result)
customer

# 현재 메모리상에 생성되어 있는 다양한 R 객체 조회
ls()

# 여러 데이터 객체를 활용한 리스트 데이터 객체 정의
mydata <- list(my1 = a, my2 = h3, my3 = mxr, my4 = out, my5 = customer)
mydata
class(mydata)

# 리스트객체 이용가능한 요소항목 이름파악
names(mydata)

# 리스트객체 구조파악
str(mydata)


# --------------------------------------------------
# 리스트객체 인덱싱
# --------------------------------------------------

# 리스트객체에서 my1 리스트항목(스칼라) 조회
mydata$my1
mydata[['my1']]
mydata[[1]]

# 리스트객체에서 my2 리스트항목(벡터) 조회
mydata$my2
mydata[['my2']]
mydata[[2]]

mydata[[2]][3]
mydata[[2]][c(4, 2)]
mydata[[2]][c(2:4)]

# 리스트객체에서 my3 리스트항목(행렬) 조회
mydata$my3
mydata[['my3']]
mydata[[3]]

mydata[[3]][3]
mydata[[3]][1, ]
mydata[[3]][, 2]
mydata[[3]][, c(4, 2)]

# 리스트객체에서 my4 리스트항목(배열) 조회
mydata$my4
mydata[['my4']]
mydata[[4]]

mydata[[4]][, , 1]
mydata[[4]][, 2, 1]
mydata[[4]][4, , 1]
mydata[[4]][4, 2, 3]

# 리스트객체에서 my5 리스트항목(데이터프레임) 조회
mydata$my5
mydata[['my5']]
mydata[[5]]

mydata[[5]][, 4]
mydata[[5]][2, ]
mydata[[5]][2, 4]

mydata[[5]][, c(4, 2)]
mydata[[5]][c(2:5), ]

 
### End of Source ####################################################


