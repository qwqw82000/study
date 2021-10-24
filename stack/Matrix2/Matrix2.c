#include<stdio.h>
#include<stdlib.h>

#define ROWS 3
#define COLS 3
#define MAX_TERMS 10
//define element
typedef struct {
	int row;
	int col;
	int value;
}element;
//define SparseMatrix
typedef struct SparseMatrix{
	element data[MAX_TERMS];
	int rows;
	int cols;
	int terms;
}SparseMatrix;

SparseMatrix sparse_matrix_add2(SparseMatrix a, SparseMatrix b){
	SparseMatrix c;
	//ca is index of a
	//cb is index of b
	//cc is index of c
	int ca =0, cb =0, cc = 0;
	if(a.rows != b.rows || a.cols != b.cols){
		fprintf(stderr, "Matrix size error\n");
		exit(1);
	}
	//define c's row,col,terms
	c.rows = a.rows;
	c.cols = a.cols;
	c.terms = 0;
	while(ca < a.terms && cb < b.terms){
		int inda = a.data[ca].row*a.cols + a.data[ca].col;
		int indb = b.data[cb].row * b.cols + b.data[cb].col;
		if(inda < indb)
			c.data[cc++] = a.data[ca++];
		else if (inda == indb){
			if(a.data[ca].value + b.data[cb].value != 0){
				//now c<- a's row,col  and (a+b)  value 
				c.data[cc].row = a.data[ca].row;
				c.data[cc].col = a.data[ca].col;
				c.data[cc++].value = a.data[ca++].value + b.data[cb++].value;
			}
			else{
				ca++;
				cb++;
			}
		}
		//inda>indb
		else c.data[cc++] = b.data[cb++];
	}
	for(; ca<a.terms;)
		c.data[cc++] = a.data[ca++];
	for (; cb<b.terms;)
		c.data[cc++] = b.data[cb++];
	c.terms = cc;

	return c;
}

void print(SparseMatrix mat){
	int i,j,mat_term = 0;
	
	for(i=0;i<ROWS;i++){
		for(j=0;j<COLS;j++){
			if(i == mat.data[mat_term].row && j == mat.data[mat_term].col)
				printf("%d",mat.data[mat_term++].value);
			else
				printf("0 ");
		}
		printf("\n");
	}
}

int main()
{
	SparseMatrix m1 = {{{1,1,5},{2,3,9}},3,3,2};
	SparseMatrix m2 = {{{0,0,5},{2,2,9}},3,3,2};
	SparseMatrix m3;

	printf("matrix m1: \n");
	print(m1);
	printf("matrix m2: \n");
	print(m2);
	m3 = sparse_matrix_add2(m1,m2);
	printf("matrix m3: \n");
	print(m3);
}
