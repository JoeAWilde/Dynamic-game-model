#include <iostream>
#include "functions.h"

double HighTide(int tides, int tide, int e, double ***fitness, double *HTCprob, int HTcost, bool stochasticHTcost)
{
    double htFit = 0;
    if(stochasticHTcost == true)
    {
        for(int i=0; i<3; i++)
        {
            int eIndex = e - (HTcost-2+i);
            if(eIndex<0) eIndex = 0;
            htFit += (fitness[tide+1][eIndex][0]*HTCprob[i]);
        }
    }
    else
    {
        int eIndex = e - HTcost;
        if(eIndex<0) eIndex = 0;
        htFit = fitness[tide+1][eIndex][0];
    }
    return htFit;
}