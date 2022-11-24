#ifndef OTHER_FILE
#define OTHER_FILE
#include "arrayContainer.h"

ArrayContainer* ThreeDarray(int tides, int tSteps, int eMax);

int Fact(int x);

double*** StartingStrat(double*** resiStrategy, int tides, int eMax, int tSteps);

double HighTide(int tides, int tide, int e, double ***fitness, double *HTCprob, int HTcost, bool stochasticHTcost);

double TimeoutFitness(int tide, int t, int e, double* metCostProb, double ***timeoutFitness, double ***BRfitness, double ptau, double pMort);

double WaveFitness(int tide, int t, int e, double ***timeoutFitness, double ***BRfitness, int waveCost, double* metCostProb, double **pMate, int mateBonus, double pMort, double pWaveMort, bool postMatingTimeout);

double ForageFitness(int tide, int t, int e, int eMax, double ***BRfitness, int n, double *binom, double pMort, double* metCostProb);

ArrayContainer* Markov(int counter,
                int tides,
                int eMax, 
                int tSteps, 
                int HTcost,
                double* metCostProb,
                int waveCost,
                int n,
                bool stochasticHTcost,
                double b, 
                double alpha, 
                double ptau,
                bool postMatingTimeout,     
                double* binom,
                double** Rt,
                double** pMate,
                double** pFemMaxList, 
                double** pFemMinList,
                double* HTCprob,
                double*** resiStrategy, 
                double**** freqDist,
                double pMort, 
                double pWaveMort);



#endif
