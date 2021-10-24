#include<stdio.h>
#include<stdlib.h>
#include<time.h>

#define N 5

void matmult(int a[][N], int b[][N], int c[][N]);
void print_ary(int c[][N],const char* name);
int main()
{
	clock_t start,finish;
	double duration;
	int i, j;
	int a[N][N], b[N][N], c[N][N];

	for (i = 0;i < N; i++)
		for (j = 0; j < N; j++){
			a[i][j] = rand() % 10;
			b[i][j] = rand() % 10;
		}
	start = clock();
	matmult(a,b,c);
	finish = clock();
	duration =(double)(finish - start) / CLOCKS_PER_SEC;
	print_ary(a,"a");
	print_ary(b,"b");
	print_ary(c,"c");
	printf("%f seconds\n", duration);
}
void matmult(int a[][N], int b[][N], int c[][N]){
		int i,j,k;

		for(i = 0; i<N;i++){
			for(j = 0; j < N; j++){
				c[i][j] += a[i][k] * b[k][j];
				}
	}
}
void print_ary(int c[][N],const char* name){
	int i,j;
	printf("%s ary is: \n",name);

	for(i = 0; i < N; i++){
		for(j = 0; j < N; j++){
			printf("%10d ",c[i][j]);
		}
		printf("\n");
	}
}

				
