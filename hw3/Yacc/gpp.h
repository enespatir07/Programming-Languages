#ifndef GPP_H
#define GPP_H

typedef struct Valuef
{
    int num;   /* numerator */
    int denom; /* denominator */
} Valuef;



Valuef convertStrToValuef(char *);

Valuef createValuef(int, int);

Valuef addValuef(Valuef, Valuef);

Valuef subValuef(Valuef, Valuef);

Valuef multValuef(Valuef, Valuef);

Valuef divValuef(Valuef, Valuef);

#endif