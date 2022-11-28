#include <iostream>
#include "functions.h"

double WaveFitness(int tide, int t, int e, double ***timeoutFitness, double ***BRfitness, int waveCost, double* metCostProb, double **pMate, int mateBonus, double pMort, double pWaveMort, bool postMatingTimeout)
{

    /* std::cout << "tide = " << tide << "\n";
    std::cout << "t = " << t << "\n";
    std::cout << "e = " << e << "\n"; */
    double wM = 0.0;
    double wI = 0.0;
    double wTotal = 0.0;

    //std::cout << "pMate[" << tide << "][" << t << "] = " << pMate[tide][t] << "\n\n";

    for(int m=0; m<3; m++)
    {
        int eWave = e - waveCost - m;
        if(eWave<0) eWave = 0;
        
        if(postMatingTimeout == true)
        {
            wM = timeoutFitness[tide][eWave][t+1]; //fitness if you mate
        }else{
            wM = BRfitness[tide][eWave][t+1]; //fitness if you mate
        }
        wI = BRfitness[tide][eWave][t+1]; //fitness if you don't mate

        //std::cout << "BRfitness[" << tide << "][" << eWave << "][" << t+1 << "] = " << BRfitness[tide][eWave][t+1] << "\n";

        

        wTotal += ((pMate[tide][t] * (wM + mateBonus)) + ((1-pMate[tide][t]) * wI)) * metCostProb[m];

        
    }

    double wGrandTotal = (1-(pMort*pWaveMort)) * wTotal;

    
    /* std::cout << "grandTotal for wavefit = " << wGrandTotal << "\n\n"; */
    

    return wGrandTotal;
}