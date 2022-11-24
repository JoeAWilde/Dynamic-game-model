#include <iostream>
#include "functions.h"

double ForageFitness(int tide, int t, int e, int eMax, double ***BRfitness, int n, double *binom, double pMort, double* metCostProb)
{
    double kFit = 0;

    /* if(t>20 & t<=30)
    {
        for(int m=0; m<3; m++)
        {
            int eIndex = e - m;
            if(eIndex < 0) eIndex = 0;
            kFit += BRfitness[tide][eIndex][t+1] * metCostProb[m];
        }
    }else
    { */
    for(int k=0; k <= n; k++)
    {
        for(int m=0; m<3; m++)
        {
            int eIndex = e + k - m;
            if(eIndex < 0) eIndex = 0;
            if(eIndex >= eMax) eIndex = eMax-1; 

            

            kFit += BRfitness[tide][eIndex][t+1] * binom[k] * metCostProb[m];
        }
    }
    //}
    double totalkFit = (1-pMort) * kFit;
    return totalkFit;
}