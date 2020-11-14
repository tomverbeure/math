#include <stdlib.h>
#include <stdio.h>
#include <math.h>

// Code of this tweet: https://twitter.com/field_hamster/status/1326800952586874883

u_int32_t hamster_sqrt(u_int32_t i)
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

u_int32_t fpga_sqrt(u_int32_t d)
{
	u_int32_t q = 0;
	int32_t r = 0;

	for(int k=15; k>= 0; --k){
		if (r >= 0){
			r = ((r<<2) | ((d >> (2*k)) & 3)) - ((q<<2) | 1);
		}
		else{
			r = ((r<<2) | ((d >> (2*k)) & 3)) + ((q<<2) | 3);
		}

		if (r >= 0){
			q = (q<<1) | 1;
		}
		else{
			q = (q<<1);
		}
	}

	if (r < 0){
		r = r + ((q<<1) | 1);
	}

	return q;
}


int main()
{
	srand(0);

	for(int i=0; i<100; ++i){
		u_int32_t op;

		op = rand() & 0xffffff;
		u_int32_t result_ref = (u_int32_t)sqrt(op);
		//u_int32_t result_dut = hamster_sqrt(op);
		u_int32_t result_dut = fpga_sqrt(op);

		if (result_ref != result_dut){
			printf("op (%d): ref (%d) != dut (%d)\n", op, result_ref, result_dut);
		}
	}
}

