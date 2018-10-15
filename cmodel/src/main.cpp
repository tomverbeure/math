
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#include <iostream>

#include <fpxx.h>
#include <misc.h>

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



// Input: integer
// Output: integer with 16 fractional bits
unsigned int sqrt_int(unsigned int s)
{
    int lz = leading_zeros_int(s);
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

int float_mant(float f)
{
    return float_as_int(f) & 0x7fffff;
}

float random_float()
{
    return int_as_float(random());
}

bool check_normal(float f)
{
    bool normal = isnormal(f) || (f == 0.0f);

    return normal;
}

void print_bits_float(float f)
{
    unsigned int fi = float_as_int(f);

    printf("%d ", fi>>31);

    for(int i=30;i>=23;--i){
        printf("%d", (fi>>i)&1);
    }
    printf(" ");
    for(int i=22;i>=0;--i){
        printf("%d", (fi>>i)&1);
    }
}

void print_bits_double(double f)
{
    uint64_t fi = double_as_long(f);

    printf("%lld ", fi>>63);

    for(int i=62;i>=52;--i){
        printf("%lld", (fi>>i)&1);
    }
    printf(" ");
    for(int i=51;i>=0;--i){
        printf("%lld", (fi>>i)&1);
    }
}

template <int _m_size, int _exp_size, int _zero_offset = ((1L<<(_exp_size-1))-1)>
void print_fp32_fpxx(float fp32, fpxx<_m_size, _exp_size, _zero_offset> fx)
{
    cout << "fp32: "; print_bits_float(fp32);
    printf("\n");
    cout << "fpxx: "; fx.print_bits();
    printf("\n");
}


bool stress_fpxx()
{
    // Check that fpxx<23,8> has the same results as fp32 for regular numbers (no denormals)
    fpxx<23,8> fpxx_a;
    fpxx<23,8> fpxx_b;
    fpxx<23,8> fpxx_r;

    for(long int i=0;i<10000000;++i){
        float fp32_a = random_float();
        float fp32_b = random_float();
        float fp32_r;
        double fp64_a;
        double fp64_b;
        double fp64_r;

        if (i==2){
            continue;
        }

        if (false){
            // Test special cases
            fp32_a = int_as_float((float_as_int(fp32_a) & 0xff800400) | 0x00000000);
            fp32_b = int_as_float((float_as_int(fp32_b) & 0xff800400) | 0x00000400);
        }

        fpxx_a = fp32_a;
        fpxx_b = fp32_b;

        fp64_a = fp32_a;
        fp64_b = fp32_b;

        if (!check_normal(fp32_a) || !check_normal(fp32_b) || fpxx_b.is_zero() ){
            continue;
        }

        fp32_r = fp32_a / fp32_b;
        fp64_r = fp64_a / fp64_b;

        if (!check_normal(fp32_r)){
            continue;
        }

        fpxx_r = fpxx_a / fpxx_b;

        //if (fp32_r != fpxx_r && abs(fpxx_r.mant() - float_mant(fp32_r)) > 2){
        if (fp32_r != fpxx_r && abs(float_mant((float)(fpxx_r)) - float_mant(fp32_r)) > 0){
            printf("Mismatch: %ld: fp32 %15e != fpxx %15e\n", i, fp32_r, (float)fpxx_r);
            printf("fp32_a: "); print_bits_float(fp32_a); printf(" %16e\n", fp32_a);
            printf("fpxx_a: "); print_bits_float(fp32_a); printf(" %16e, exp: %3d, mant: %8d\n", (float)fpxx_a, fpxx_a.exp, fpxx_a.mant());
            printf("\n");
            printf("fp32_b: "); print_bits_float(fp32_b); printf(" %16e\n", fp32_b);
            printf("fpxx_b: "); print_bits_float(fp32_b); printf(" %16e, exp: %3d, mant: %8d\n", (float)fpxx_b, fpxx_b.exp, fpxx_b.mant());
            printf("\n");
            printf("fp32_r:    "); print_bits_float(fp32_r); printf(" %16e\n", fp32_r);
            printf("fpxx_r:    "); print_bits_float(fpxx_r); printf(" %16e, exp: %3d, mant: %8d\n", (float)fpxx_r, fpxx_r.exp, fpxx_r.mant());
            printf("\n");
            printf("fp64_r: "); print_bits_double(fp64_r); printf(" %16e\n", fp64_r);
            printf("fpxx_r: "); print_bits_double(fpxx_r.to_double()); printf(" %16e, exp: %3d, mant: %8d\n", (double)fpxx_r, fpxx_r.exp, fpxx_r.mant());
            printf("           ");
            fpxx_r.print_bits();
            printf("\n");
            assert(0);
        }
    }

    return true;
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


//    stress_fpxx();

    fpxx<23,8> my_fp, left, right;

#if 1
    left  = 1; right = 100000000;
    my_fp = left / right;
    cout << left << "/" << right << "=" << my_fp << "(" << (float)left/(float)right << ")" <<  endl;

    left  = 1.999; right = 1.999;
    my_fp = left / right;
    cout << left << "/" << right << "=" << my_fp << "(" << (float)left/(float)right << ")" <<  endl;

    left  = 4; right = 2;
    my_fp = left / right;
    cout << left << "/" << right << "=" << my_fp << "(" << (float)left/(float)right << ")" <<  endl;

    left  = 123456; right = 54321;
    my_fp = left / right;
    cout << left << "/" << right << "=" << my_fp << "(" << (float)left/(float)right << ")" <<  endl;

    left  = 3.5; right = 1.5;
    my_fp = left / right;
    cout << left << "/" << right << "=" << my_fp << "(" << (float)left/(float)right << ")" <<  endl;

    left  = 3; right = 1;
    my_fp = left / right;
    cout << left << "/" << right << "=" << my_fp << "(" << (float)left/(float)right << ")" <<  endl;

    left  = 1; right = 2;
    my_fp = left / right;
    cout << left << "/" << right << "=" << my_fp << "(" << (float)left/(float)right << ")" <<  endl;
#endif

#if 0
    left = 3.494705e-06;
    right = -4.046430e-06;
    my_fp = left + right;
    float fp32 = (float)left + (float)right;
    cout << left << " + " << right << " = " << my_fp << " (" << fp32 << ")" <<  endl;

    printf("Left:\n");
    print_fp32_fpxx((float)left, left);
    printf("Right:\n");
    print_fp32_fpxx((float)right, right);
    printf("Result:\n");
    print_fp32_fpxx(fp32, my_fp);
#endif

#if 0
    cout << "--------------" << endl;
    left = 0.5;
    cout << left << " r_sqrt: " << 1.0/sqrt((float)left) << "," << recip_sqrt(left) << endl;

    cout << "--------------" << endl;
    left = 1;
    cout << left << " r_sqrt: " << 1.0/sqrt((float)left) << "," << recip_sqrt(left) << endl;

    cout << "--------------" << endl;
    left = 1.5;
    cout << left << " r_sqrt: " << 1.0/sqrt((float)left) << "," << recip_sqrt(left) << endl;

    cout << "--------------" << endl;
    left = 1.999;
    cout << left << " r_sqrt: " << 1.0/sqrt((float)left) << "," << recip_sqrt(left) << endl;

    cout << "--------------" << endl;
    left = 2;
    cout << left << " r_sqrt: " << 1.0/sqrt((float)left) << "," << recip_sqrt(left) << endl;

    left = 100;
    cout << left << " r_sqrt: " << 1.0/sqrt((float)left) << "," << recip_sqrt(left) << endl;
    left = 150;
    cout << left << " r_sqrt: " << 1.0/sqrt((float)left) << "," << recip_sqrt(left) << endl;
    left = 190;
    cout << left << " r_sqrt: " << 1.0/sqrt((float)left) << "," << recip_sqrt(left) << endl;
    left = 1000;
    cout << left << " r_sqrt: " << 1.0/sqrt((float)left) << "," << recip_sqrt(left) << endl;
    left = 1500;
    cout << left << " r_sqrt: " << 1.0/sqrt((float)left) << "," << recip_sqrt(left) << endl;
    left = 10000;
    cout << left << " r_sqrt: " << 1.0/sqrt((float)left) << "," << recip_sqrt(left) << endl;

    cout << endl;
#endif

#if 0
    cout << my_fp.m_size() << "," << my_fp.exp_size() << endl;

    my_fp = 0;
    cout << "zero:" << my_fp << endl;


    left = 10;
    right = -0.11;

    cout << left + right << endl;


    float f = 0.00001;
    for(int i=0;i<12;++i){
        my_fp = f;
        cout << f << "," << my_fp << " " << (fabs(f/(float)my_fp)-1.0) << endl;
        f *= 10;
    }
#endif
}


