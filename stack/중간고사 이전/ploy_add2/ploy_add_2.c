#include<stdio.h>
#include<stdlib.h>
#define MAX_TERMS 101
//define struct
struct poly{
	float coef;
	int expon;
};
struct poly terms[MAX_TERMS] = { {8,3},{7,1},{1,0},{10,3},{3,2},{1,0} };
int avail = 6;

//compare between a and b
char compare(int a,int b){
	if (a>b) return '>';
	else if (a==b) return '=';
	else return '<';
}
//attach c<- a or b
void attach(float coef, int expon){
	if (avail > MAX_TERMS){
		fprintf(stderr, "There are too many terms in a polynomial\n");
		exit(1);
	}

	terms[avail].coef = coef;//attach coef
	terms[avail++].expon = expon;//attach expon
}

void poly_add2(int As, int Ae, int Bs, int Be, int* Cs, int* Ce){
	float tempcoef;
	*Cs =avail;

	while(As <= Ae && Bs <= Be)
		switch (compare(terms[As].expon, terms[Bs].expon)) {//compare expon
			case '>': //A expon > B expon
				attach(terms[As].coef, terms[As].expon);
				As++;
				break;
			case '=': //A expon == B expon
				tempcoef = terms[As].coef + terms[Bs].coef;//tempcoef <-add coef
				if (tempcoef)//add result is not 0
					attach(tempcoef,terms[As].expon);
				As++;Bs++;
				break;
			case '<': //A expon < B expon
				attach(terms[Bs].coef,terms[Bs].expon);
				Bs++;
				break;
		}
	for(;As<=Ae;As++)//when end while. there are extra A in terms
		attach(terms[As].coef,terms[As].expon);//arrach C <-A
	for(; Bs <=Be;Bs++)//when end while. there are extra B in terms
		attach(terms[Bs].coef,terms[Bs].expon);
	*Ce = avail -1;
}
//print terms
void print(int first, int last){
	int i;
	for (i=first; i<=last;i++){
		printf("%.2f",terms[i].coef);
		if (terms[i].expon != 0){
			printf("x^%d",terms[i].expon);
			printf("+ ");
		}
	}
	printf("\n");
}

int main()
{
	printf("polynomial a = ");
	print(0,2);
	printf("polynomial b = ");
	print(3, 5);

	int Cs, Ce;
	poly_add2(0,2,3,5,&Cs,&Ce);

	printf("polynomial c = ");
	print(Cs, Ce);
}




