while(True):
    a = int(input("첫 번째 숫자 입력: "))
    b = int(input("두 번째 숫자 입력: "))
    ch = input("계산할 연산자 입력")

    if (ch == "+"):
        print("%d + %d = %d 입니다. " %(a,b,a+b))
    elif (ch == "-"):

        print("%d - %d = %d 입니다. " %(a,b,a-b))
    elif (ch == "*"):
        print("%d * %d = %d 입니다. " %(a,b,a*b))
    elif (ch == "/"):
        print("%d / %d = %f 입니다. " %(a,b,a/b))#나누기가 // 계산처럼 포메팅이 되어서 %f 사용
    elif (ch == "%"):
        print("%d %% %d = %d 입니다. " %(a,b,a%b))
    elif (ch == "//"):
        print("%d // %d = %d 입니다. " %(a,b,a//b))
    elif (ch == "**"):
        print("%d ** %d = %d 입니다. " %(a,b,a**b))
    

