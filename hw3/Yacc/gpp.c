#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gpp.h"

Valuef convertStrToValuef( char *str) {
    Valuef v;
    
     char *b_position = strchr(str, 'b');

    if (b_position == NULL) {
        v.num = 0;
        v.denom = 0;
        return v;
    }

    size_t num_len = b_position - str;
    size_t denom_len = strlen(b_position + 1);

    char num_str[num_len + 1];
    char denom_str[denom_len + 1];

    strncpy(num_str, str, num_len);
    num_str[num_len] = '\0';
    
    strcpy(denom_str, b_position + 1);

    v.num = atoi(num_str);
    v.denom = atoi(denom_str);

    return v;
}


Valuef createValuef(int num, int denom)
{
    Valuef v;
    v.num = num;
    v.denom = denom;
    return v;
}

int gcd(int a, int b)
{
    return a == 0 ? b : gcd(b % a, a);
}

void simplify(Valuef *v)
{
    int div = gcd(v->num, v->denom);
    v->num = v->num / div;
    v->denom = v->denom / div;
}

Valuef addValuef(Valuef v1, Valuef v2)
{
    Valuef r;
    r.num = v1.num * v2.denom + v2.num * v1.denom;
    r.denom = v1.denom * v2.denom;
    simplify(&r);
    return r;
}

Valuef subValuef(Valuef v1, Valuef v2)
{
    Valuef r;
    r.num = v1.num * v2.denom - v2.num * v1.denom;
    r.denom = v1.denom * v2.denom;
    
    simplify(&r);
    return r;
}

Valuef multValuef(Valuef v1, Valuef v2)
{
    Valuef r;
    r.num = v1.num * v2.num;
    r.denom = v1.denom * v2.denom;
    simplify(&r);
    return r;
}

Valuef divValuef(Valuef v1, Valuef v2)
{
    Valuef r;
    r.num = v1.num * v2.denom;
    r.denom = v1.denom * v2.num;
    simplify(&r);
  

    return r;
}