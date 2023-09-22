#include<math.h>
#include <stdlib.h>
#include<stdio.h>

#define n 1000
#define p 100000
#define num_nonzero 10
#define beta 2

double randn(double, double);

void main() {
	FILE *fx = fopen("x_e3_e5.txt", "a+");
	FILE *fy = fopen("y_e3_e5.txt", "a+");
	
	int i, j;
	double y = 0;
	
	for (j = 0; j < n; j++) {
		double *x = malloc(p * sizeof(*x));
		for (i = 0; i < p; i++) {
			x[i] = randn(0, 1);
			if (i < num_nonzero) {
				y = y + x[i] * beta;
			} 
			fprintf(fx, "%f\t", x[i]);
		}  
		fprintf(fx, "\n");
		fprintf(fy, "%f\n", y);
		y = 0;
		free(x);
	}
	fclose(fx);
	fclose(fy);
}

double randn (double mu, double sigma) {
  double U1, U2, W, mult;
  static double X1, X2;
  static int call = 0;

  if (call == 1)
	{
	  call = !call;
	  return (mu + sigma * (double) X2);
	}

  do
	{
	  U1 = -1 + ((double) rand () / RAND_MAX) * 2;
	  U2 = -1 + ((double) rand () / RAND_MAX) * 2;
	  W = pow (U1, 2) + pow (U2, 2);
	}
  while (W >= 1 || W == 0);

  mult = sqrt ((-2 * log (W)) / W);
  X1 = U1 * mult;
  X2 = U2 * mult;

  call = !call;

  return (mu + sigma * (double) X1);
}
