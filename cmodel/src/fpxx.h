
template <int _m_size, int _exp_size, int _zero_offset = ((1<<(_exp_size-1))-1)>
class fpxx {

public:
    const int exp_size      = _exp_size;
    const int m_size        = _m_size;
    const int zero_offset   = _zero_offset;

    bool        sign;
    unsigned    exp;
    unsigned    m;

    fpxx() {
        sign    = 0;
        exp     = 0;
        m       = 0;
    }

    operator float () {
        int e = exp == 0 ? 0 : (exp - zero_offset + ((1<<7)-1) ) & 0xff;

        union {
            float       f;
            unsigned    i;
        } fi;

        fi.i = (sign << 31) | (e<<23) | (m << (23-m_size));

        return fi.f;
    }

    fpxx operator=(float f) {

        union {
            float       f;
            unsigned    i;
        } fi;

        fi.f = f;

        bool     f_s    = fi.i >> 31;
        unsigned f_e    = (fi.i >> 23) & 0xff;
        unsigned f_m    = fi.i & 0x7fffff;

        sign    = f_s;
        exp     = f_e == 0 ? 0 : (f_e - ((1<<7)-1) + zero_offset) ;
        exp     = exp << (32-exp_size) >> (32-exp_size);
        m       = f_m >> (23-m_size);

        return *this;
    }

    bool is_zero() {
        return exp == 0;
    }

    friend fpxx<_m_size, _exp_size, _zero_offset>  operator+(const fpxx<_m_size, _exp_size, _zero_offset> left, const fpxx<_m_size, _exp_size, _zero_offset> right) {
    }

    void print_bits() {
        printf("%d ", sign);

        for(int i=exp_size-1;i>=0;--i){
            printf("%d", (exp>>i)&1);
        }
        printf(" ");
        for(int i=m_size-1;i>=0;--i){
            printf("%d", (m>>i)&1);
        }
    }

};

