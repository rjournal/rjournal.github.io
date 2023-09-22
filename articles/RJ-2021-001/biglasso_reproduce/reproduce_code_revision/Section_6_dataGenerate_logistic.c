#include<math.h>
#include <stdlib.h>
#include<stdio.h>
#include <time.h>

#define beta 2

double randn(double, double);
int rand_bernoulli(double);

int main(int argc, char *argv[]) {
	
	if (argc != 4) {
		printf("usage: %s n p nonzero (Note: nonzero must be an even number.)\n", argv[0]);
		exit(0);
	} else {
		int n, p, num_nonzero;
		n = atoi(argv[1]);
		p = atoi(argv[2]);
		num_nonzero = atoi(argv[3]);
		if (num_nonzero % 2 != 0) {
			printf("The number of non-zero coefficients must be even!\n");
			exit(0);
		}
		printf("Input:\n\tn = %d \t p = %d \t num_nonzero = %d \n", n, p, num_nonzero);
		// printf("n = %s \t p = %s \t num_nonzero = %s \n", argv[1], argv[2], argv[3]);
		
		char xfname[100] = "";
		snprintf(xfname, sizeof(xfname), "./X_%s_%s_%s_logistic.txt", argv[1], argv[2], argv[3]);
		char yfname[100] = "";
		snprintf(yfname, sizeof(yfname), "./y_%s_%s_%s_logistic.txt", argv[1], argv[2], argv[3]);
		printf("\txfname = %s \t yfname = %s \n\n", xfname, yfname);
		
		FILE *fx = fopen(xfname, "w"); 
		FILE *fy = fopen(yfname, "w");
		
		int i, j, y;
		
		printf("Simulating design matrix X and response vector y ... \n");
		
		// record time
		clock_t start, end;
		double cpu_time_used;
		start = clock();
		
		for (j = 0; j < n; j++) {
			if ((j+1) % 100 == 1) {
				printf("\tSimulating data for observation: %d\n", j+1);
			}
			double *x = malloc(p * sizeof(*x));
      double sum_x = 0.0;
      double prob = 0.0;
      
			for (i = 0; i < p; i++) {
				x[i] = randn(0, 1);
				if (i < num_nonzero / 2) {
					sum_x = sum_x + x[i] * beta;
				}
				if (i >= num_nonzero /2 && i < num_nonzero) {
					sum_x = sum_x - x[i] * beta;
				}
				if (i != p-1) {
					fprintf(fx, "%f,", x[i]);
				} else {
					fprintf(fx, "%f", x[i]);
				}					
			}
      // compute prob. and get y
      prob = exp(sum_x) / (1 + exp(sum_x));
      y = rand_bernoulli(prob);
      // printf("\t\t exp(sum_x) = %f; sum_x = %f; prob = %f; y = %d\n", exp(sum_x), sum_x, prob, y);

			fprintf(fx, "\n");
			fprintf(fy, "%d\n", y);

			free(x);
		}
		fclose(fx);
		fclose(fy);
		
		// end time
		end = clock();
		cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
		printf("DONE!\n\n");
		
		printf("start = %.20f\nend   = %.20f\n", start, end);
		printf("delta = %.20f\n", ((double) (end - start)));
		printf("cpu_time_used  = %.15f\n", cpu_time_used);
		printf("CLOCKS_PER_SEC = %i\n\n", CLOCKS_PER_SEC);

	}
}

int rand_bernoulli(double p) {
  int out;
  if (p <= 0) out = 0;
  if (p >= 1) out = 1;
  double r = (double) rand() / RAND_MAX;
  if (p <= r) {
    out = 1;
  } else {
    out = 0;
  }
  return out;
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
