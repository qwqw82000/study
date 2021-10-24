#include<stdio.h>
#include<stdlib.h>
#include<time.h>

#define N 5

void matadd(int a[][N], int b[][N], int c[][N]);

int main(void)
{
	int i,j;
	clock_t start, finish;
	double duration;
	int a[N][N],b[N][N],c[N][N];

	for( i = 0; i < N; i++)
		for( j = 0; j < N; j++){
			a[i][j] = rand() % 10;
			b[i][j] = rand() % 10;
		}
	start = clock();
	matadd(a,b,c);
	finish = clock();
	duration = (double)(finish - start) / CLOCKS_PER_SEC;
	printf("%f seconds\n", duration);
	return 0;
}

void matadd(int a[][N], int b[][N], int c[][N]){
	int i,j;
	for(i=0;i<N;i++)
		for(j=0;j<N;j++)
			c[i][j] = a[i][j] + b[i][j];
}
 
