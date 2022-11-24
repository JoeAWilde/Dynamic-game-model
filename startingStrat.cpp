#include <iostream>
#include "functions.h"

double*** StartingStrat(double*** resiStrategy, int tides, int eMax, int tSteps)
{
    for(int tide=0; tide<tides; tide++)
    {
        for(int t=0; t<tSteps; t++)
        {
            for(int e=0; e<eMax; e++)
            {
                resiStrategy[tide][e][t] = 0.5;
            }
        }
    }
   return resiStrategy;
}
