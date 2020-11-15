#include <stdlib.h>
#include <stdio.h>
#include <math.h>

// Code of this tweet: https://twitter.com/field_hamster/status/1326800952586874883

u_int32_t sqrt_hamster(u_int32_t i)
{
	int32_t n = 0;
	int bit = 0x8000;
	int delta_part_a = 0;
	int delta_part_b = bit*bit;

	while(bit > 0){
		int temp = i - (delta_part_a + delta_part_b);
		delta_part_a >>= 1;
		if (temp >= 0){
			i = temp;
			delta_part_a |=  delta_part_b;
			n |= bit;
		}
		delta_part_b >>= 2;
		bit >>= 1;
	}

	return n;
}

// Code based on "Implementation of Single Precision Floating Point Square Root on FPGAs"
// Non-Restoring Square Root Algorithm
// https://yamin.cis.k.hosei.ac.jp/papers/FCCM97.pdf

u_int32_t sqrt_yamin_rev1(u_int32_t d)
{
	u_int32_t q = 0;
	int32_t r = 0;

	for(int k=15; k>=0; --k){
		if (r >= 0)
			r = ((r<<2) | ((d >> (2*k)) & 3)) - ((q<<2) | 1);
		else
			r = ((r<<2) | ((d >> (2*k)) & 3)) + ((q<<2) | 3);

		q = (q<<1) | (r >= 0);
	}

	return q;
}

u_int32_t sqrt_yamin_rev2(u_int32_t d)
{
	u_int32_t q = 0;
	int32_t r = 0;

	for(int k=15; k>=0; --k){
		if (r >= 0)
			r = ((r<<2) | ((d >> 30) & 3)) - ((q<<2) | 1);
		else
			r = ((r<<2) | ((d >> 30) & 3)) + ((q<<2) | 3);
		d <<= 2;

		q = (q<<1) | (r >= 0);
	}

	return q;
}

u_int32_t sqrt_yamin_tom(u_int32_t d)
{
	u_int32_t q = 0;
	int32_t r = 0;

	for(int k=15; k>=0; --k){
		if (r >= 0){
			r = ((r<<2) | ((d >> (2*k)) & 3)) - ((q<<2) | 1);
			q = (q<<1) | 1;
		}
		else{
			r = (r<<2) + ((d >> (2*k)) & 3);
			q = (q<<1);
		}
	}

	return q;
}


// An Efficient Implementation of the Non Restoring Square Root Algorithm in Gate Level
// http://www.ijcte.org/papers/281-G850.pdf
// Code is based on VHDL in that paper.

u_int32_t sqrt_sutikno(u_int32_t d)
{
	u_int32_t q = 0;
	int32_t r = 0;
	int32_t remain = 0;

	for(int k=15; k>=0; --k){

		if (remain >= 0){
			r = (remain<<2) | ((d >> (2*k)) & 3);
		}
		else{
			r = (r<<2) | ((d >> (2*k)) & 3);
		}

		u_int32_t q_guess = (q<<2) | 1;
		remain = r - q_guess;
		q = (q<<1) | (remain>=0);
	}

	return q;
}

int bench(int nr_loops, int buf_size, u_int32_t (*sqrt)(u_int32_t))
{
	u_int32_t result = 0;

	u_int32_t *buf = malloc(buf_size * sizeof(u_int32_t));
	for(int i=0; i<buf_size; ++i){
		buf[i] = rand() & 0xffffffff;
	}

	for(int i=0;i<nr_loops;++i){
		for(int j=0;j<buf_size;++j){
			result += sqrt(buf[j]);
		}
	}

	return result;
}

int test(int nr_loops, u_int32_t (*dut_sqrt)(u_int32_t))
{
	for(int i=0; i<nr_loops; ++i){
		u_int32_t op = rand() & 0xfffffff;
		u_int32_t result_ref = (u_int32_t)sqrt(op);
		u_int32_t result_dut = dut_sqrt(op);

		if (result_ref != result_dut){
			printf("op (%d): ref (%d) != dut (%d)\n", op, result_ref, result_dut);
			return 0;
		}
	}

	return 1;
}

int main(int argc, char **argv)
{
	srand(0);

	int nr_loops = 100;
	int test_or_bench = 0;
	int algo = 0;

	if (argc >= 4){
		test_or_bench = atoi(argv[1]);
		algo = atoi(argv[2]);
		nr_loops = atoi(argv[3]);
	}

	printf("Type: %d - %s\n", test_or_bench, test_or_bench == 0 ? "test" : "benchmark");
	printf("Algo: %d - %s\n", algo, algo == 0 ? "hamster" : 
	                                algo == 1 ? "yamin_rev1" :
	                                algo == 2 ? "yamin_rev2" :
	                                            "sutikno" 
									);
	printf("Nr loops: %d\n", nr_loops);

	if (test_or_bench == 0){
		int result = test(nr_loops, algo==0 ? sqrt_hamster   : 
								    algo==1 ? sqrt_yamin_rev1 : 
								    algo==2 ? sqrt_yamin_rev2 : 
										      sqrt_sutikno );
		printf("test: %d", result);
	}
	else{
		bench(nr_loops, 10000, algo==0 ? sqrt_hamster   :  
						       algo==1 ? sqrt_yamin_rev1 :
						       algo==2 ? sqrt_yamin_rev2 :
							   			 sqrt_sutikno );
	}
}

