
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#include <iostream>

#include <fpxx.h>

using namespace std;

// See also: https://www.mathworks.com/help/fixedpoint/examples/implement-fixed-point-square-root-using-lookup-table.html

#define SQRT_LUT_SIZE_BITS          8
#define SQRT_LUT_SIZE               ((1<<SQRT_LUT_SIZE_BITS)-(1<<(SQRT_LUT_SIZE_BITS-2)))
#define SQRT_LUT_VAL_BITS           16
#define SQRT_FRAC_BITS              8

#define SQRT_LUT_VAL_MULT           (1<<SQRT_LUT_VAL_BITS)

// This table has square root values for all x where 0.5 <= x <= 2.0
// The +1 is added to allow easy lookup of the next value when doing interpolation.
unsigned sqrt_lut[SQRT_LUT_SIZE+1];

void init_sqrt_lut()
{
    for(int i=0;i<SQRT_LUT_SIZE+1;++i){
        sqrt_lut[i] = sqrt(((1<<(SQRT_LUT_SIZE_BITS-2)) + i)/(float)(1<<(SQRT_LUT_SIZE_BITS-1))) * (1<<SQRT_LUT_VAL_BITS);
    }
}


// https://codingforspeed.com/counting-the-number-of-leading-zeros-for-a-32-bit-integer-signed-or-unsigned/
int leading_zeros(int x)
{
    unsigned n = 0;
    const unsigned bits = sizeof(x) * 8;
    for (int i = 1; i < bits; i ++) {
        if (x < 0) break;
        n++;
        x <<= 1;
    }
    return n;
}

// Input: integer
// Output: integer with 16 fractional bits
unsigned int sqrt_int(unsigned int s)
{
    int lz = leading_zeros(s);
    int lz_adj = (lz & 1) ? lz : lz-1;

    // Significant bits
    int bits = 32-lz_adj;

    // Drop the fractional bits or add significant bits so that we fit in the range of the LUT.
    int lut_addr = (bits > SQRT_LUT_SIZE_BITS) ? s >> (bits - SQRT_LUT_SIZE_BITS) : s << (SQRT_LUT_SIZE_BITS - bits);
    // Table starts at 0.5, subtract offset.
    lut_addr = lut_addr - (1<<(SQRT_LUT_SIZE_BITS-2));

    unsigned long lut_val        = sqrt_lut[lut_addr];
    unsigned long lut_val_next   = sqrt_lut[lut_addr+1];

    unsigned int frac = (bits >= SQRT_LUT_SIZE_BITS+SQRT_FRAC_BITS) ? (s >> (bits - SQRT_LUT_SIZE_BITS - SQRT_FRAC_BITS)) & ((1<<SQRT_FRAC_BITS)-1)  :
                        (bits >= SQRT_LUT_SIZE_BITS)   ? (s << (SQRT_LUT_SIZE_BITS+SQRT_FRAC_BITS - bits))   & ((1<<SQRT_FRAC_BITS)-1) :
                                                         0;

    lut_val      = lut_val      << (bits/2);
    lut_val_next = lut_val_next << (bits/2);

    unsigned long lut_val_avg = (lut_val * ((1<<SQRT_FRAC_BITS)-frac) + lut_val_next * frac) / (1<<SQRT_FRAC_BITS);

//    lut_val_avg      = lut_val_avg      << (bits/2);

    unsigned r = lut_val_avg;

    return r;
}


float sqrt_fp32(float s)
{
    float r = sqrt_int(s* (float)(SQRT_LUT_VAL_MULT)) / sqrt(SQRT_LUT_VAL_MULT) / SQRT_LUT_VAL_MULT;
    
    return r;
}

void test_sqrt(unsigned int s)
{
    printf("%d: %f, %f\n", s, sqrt(s), sqrt_fp32(s));
}

void test_deviation()
{
    float max_dev = 0.0;
    float max_in  = 0;

    for(int i=1;i<1000000;++i){
        float f = i/65536.0;

        float s  = sqrt(f);
        float fs = sqrt_fp32(f);

        float dev = fabs((fs - s)/s);

        if (dev>max_dev){
            max_dev = dev;
            max_in  = f;
        }
    }

    printf("%f: %f, %f (%f%%)\n", max_in , sqrt(max_in), sqrt_fp32(max_in), max_dev*100);
}

int main(int argc, char **argv)
{
#if 0
    init_sqrt_lut();

    cout << endl;
    test_deviation();
    cout << endl;

    test_sqrt(16);
    test_sqrt(32);
    test_sqrt(48);
    test_sqrt(256);
    test_sqrt(511);
    test_sqrt(512);
    test_sqrt(2048);
    test_sqrt(32768);
    test_sqrt(32769);
#endif

    fpxx<17,5> my_fp;

    cout << endl;
    cout << my_fp.m_size << "," << my_fp.exp_size << endl;


    float f = 0.00001;
    for(int i=0;i<12;++i){
        my_fp = f;
        cout << f << "," << my_fp << " " << (fabs(f/my_fp)-1.0) << endl;
        f *= 10;
    }
}


