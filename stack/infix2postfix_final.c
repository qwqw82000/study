#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define MAX_STACK_SIZE 100
#define FALSE 0
#define TURE 1
typedef int element;

typedef struct {
    element stack[MAX_STACK_SIZE];
    int top;
}StackType;

void init(StackType* s){
    s->top = -1;
}

//공백 상태 검출 함수
int is_empty(StackType* s){
    return (s->top == -1);
}
//포화 상태 검출 함수
int is_full(StackType* s){
    return (s->top == (MAX_STACK_SIZE -1));
}
//삽입 함수
void push(StackType* s, element item){
    if(is_full(s)){
        fprintf(stderr, "Stack full error\n");
        return;
    }
    //현재 stack 상단에 item을 push한다.
    else s-> stack[++(s->top)] = item;
}
//삭제 함수
element pop(StackType* s){
    if(is_empty(s)){
        fprintf(stderr, "Stack is error\n");
        exit(1);
    }
    else return s->stack[(s->top)--];
}
//peek 함수
element peek(StackType* s){
    if(is_empty(s)){
        fprintf(stderr, "Stack is empty\n");
        exit(1);
    }
    else return s->stack[s->top];
}
//후위 수식 합 구하기
element eval(StackType* r){
    int op1, op2, value, i = 0;
    int len = (r->top) + 1;
    char ch;

    StackType s;
    init(&s);

    for( i = 0; i < len; i++){
        ch =(char)(r->stack[i]);
        //현재 들어오는 ch가 숫자라면---------
        if(ch != '+' && ch != '-' && ch != '*' && ch != '/'){
            value = ch - '0';
            push(&s,value);
        }
        //현재 들어오는 ch가 문자라면---------
        else{
            //최상단 top의 값 op2에 할당
            op2 = pop(&s);
            //최상단 top-1의 값 op1에 할당
            op1 = pop(&s);
            switch(ch){
                case '+': push(&s, op1 + op2); break;
                case '-': push(&s, op1 - op2); break;
                case '*': push(&s, op1 * op2); break;
                case '/': push(&s, op1 / op2); break;
            }
        }
    }
    //현재 계산한 값을 반환해준다
    //반환하게 되면 
    return pop(&s);
}
//연산자 우선순위 반환 함수
int prec(char op){
    switch (op){
        //괄호의 연산자 우선순위가 가장 낮다.
        case '(': case ')': return 0;
        //+와 -의 연산자 우선순위는 그 다음이다.
        case '+': case '-': return 1;
        //*와 /의 연산자 우선순위가 가장 높다.
        case '*': case '/': return 2;
    }
    //만약 들어온 문자가 연산자가 아닌 피 연산자(예를들어 1,2,3,4)라면 -1을 반환한다.
    return -1;
}
void print_struct(StackType* s, int len){
    printf("postfix: ");
    for (int i=0; i<(s->top) + 1 ; i++){
        printf("%c", s->stack[i]);
    }
    printf("\n");
}
//중위표기 수식을 후위 표기식으로 변환하는 함수
//예시 A+B*C-D/E
StackType infix_to_postfix (char* exp, int len){
    
    int i = 0;
    char ch, top_op;
 
    StackType r;

    StackType s;
    
    init(&s);
    init(&r);

    for (i = 0; i < len; i++){
        ch = exp[i];//A.
        switch (ch){
            //스택에 있는 연산자의 우선순위가 더 크거나 같으면 출력
            //즉 연산자가 들어왔을 경우 우선순위를 비교해서 스택에 넣은다.
            case '+': case '-': case '*': case '/':
            //스택 내부가 비어있지 않으면서
            //이전에 스택에 들어있는 최상단 연산자와 현재의 연산자를 비교했을때
            //연산순위 비교
                while(!is_empty(&s) && (prec(ch) <= prec(peek(&s))))
                    //꺼내오고
                    push(&r, pop(&s));
                    
                //집어넣기
                push(&s,ch);
                break;
            case '('://왼쪽괄호가 들어오면 바로 스택에 저장
                push(&s,ch);
                break;
            case ')'://오른쪽 괄호가 들어오면
                top_op = pop(&s);
                //스택 상단의 연산자가 '('일때까지는 모두 출력한다
                while(top_op != '('){
                    push(&r,top_op);
                    top_op = pop(&s);
                }
                break;
            default://현재 받은 문자가 피연산자일때
                push(&r, ch);
                break;
        }
    }
    //스택에 저장된 연산자들을 출력한다.
    while(!is_empty(&s)){
        push(&r,pop(&s));
    }
    return r;
}

int main()
{
    StackType result;
    int num;
    char infix[100];

    printf("infix input: ");
    scanf("%s",infix);
    int len =strlen(infix);
 
    
    
    result = infix_to_postfix(infix, len);
    print_struct(&result, len);
    num = eval(&result);
    

    printf("result is %d\n",num);
    return 0;
}