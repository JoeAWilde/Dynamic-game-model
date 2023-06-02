#include <iostream>
#include "functions.h"

double TimeoutFitness(int tide, int t, int e, double* metCostProb, double ***timeoutFitness, double ***BRfitness, double ptau, double pMort)
{
    //std::cout << "e = " << e << ", t = " << t << "\n";
    double Wtau = 0.0;
    //for(int m=0; m<3; m++)
    //{
        int eIndex = e;

        if(eIndex < 0) eIndex = 0;

        //if(e == 50 & t == 1) std::cout << "to be added = " << (ptau * BRfitness[tide][eIndex][t+1]) + ((1-ptau) * timeoutFitness[tide][eIndex][t+1]) * metCostProb[m] << "\n";

        Wtau += ((ptau * BRfitness[tide][eIndex][t+1]) + ((1-ptau) * timeoutFitness[tide][eIndex][t+1]));
    
        //if(e == 50 & t == 1) std::cout << "metCostProb[" << m << "] = " << metCostProb[m] << " , Wtau = " << Wtau << "\n\n";
    //}
    double WtauTotal = (1-pMort) * Wtau;
    return WtauTotal;
}