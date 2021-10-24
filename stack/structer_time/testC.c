#include<stdio.h>
#include<time.h>

int main()
{
	clock_t start, finish;
	double duration;
	unsigned int n;
	long res=0;

	printf("input a positive int : ");
	scanf("%d", &n);

	start = clock();

	for (int i;i<n;i++){
		for (int j;j<n;j++){
			res += 1;
		}
	}

	finish = clock();
	duration = (double)(finish - start) / CLOCKS_PER_SEC;
	printf("res = %ld (%f seconds)\n",res,duration);
}

