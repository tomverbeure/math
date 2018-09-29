
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

    operator float () const {
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

    bool is_zero() const {
        return exp == 0;
    }

    friend fpxx<_m_size, _exp_size, _zero_offset>  operator+(const fpxx<_m_size, _exp_size, _zero_offset> left, const fpxx<_m_size, _exp_size, _zero_offset> right) {
        fpxx<_m_size, _exp_size, _zero_offset> r;

        if (left.is_zero())
            return right;

        if (right.is_zero())
            return left;

        int m_left  = (1 << _m_size) | left.m;
        int m_right = (1 << _m_size) | right.m;

        int s_add;
        int e_add;
        int m_add;

        if (left.exp > right.exp){
            e_add   = left.exp;
            m_right >>= (left.exp - right.exp);
        }
        else{
            e_add   = right.exp;
            m_left >>= (right.exp - left.exp);
        }

        if (left.sign == right.sign){
            s_add = left.sign;
            m_add = m_left + m_right;
        }
        else{
            if (m_left > m_right){
                s_add = left.sign;
                m_add = m_left - m_right;
            }
            else{
                s_add = right.sign;
                m_add = m_right - m_left;
            }
        }

        if (m_add & (1<<(_m_size+1))){
            e_add += 1;
            m_add >>= 1;
        }
        else{
            while((m_add & (1<<_m_size)) == 0 && e_add != 0){
                e_add -= 1;
                m_add <<= 1;
            }
        }

        r.sign = s_add;
        r.exp  = e_add;
        r.m    = m_add & ((1<<_m_size)-1);

        return r;
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

