//Header files
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <math.h> 
#include <bits/stdc++.h>
#include <chrono>
#include <ctime>   
#include <random>
#include "functions.h"

// Initialising...
// Array dimensions
const int eMax                   = 51; //energy levels
const int tSteps                = 101; //timeSteps
const int tides                  = 60; //tides (fixed number)

// High tide cost parameters
const int HTcost                  = 10; //energetic cost of high tide
const bool stochasticHTcost     = true; //is the high tide cost fixed or stochastic

// Costs
const int waveCost                 = 5; //cost of waving
<<<<<<< Updated upstream
const double ptau                = 0.1; //probability of returning to non-timeout array
const bool postMatingTimeout    = false;
=======
const double ptau               = 0.01; //probability of returning to non-timeout array
const bool postMatingTimeout    = true;
>>>>>>> Stashed changes
const int mateBonus                = 5; //fitness bonus from mating
const double pMort           = 0.00001; //probability of death in every timeStep
const double pWaveMort             = 2; //multiplier for how much more mortality occurs when you wave

// Foraging gains parameters
const int n                        = 4; //max energy gain from foraging
const double p                   = 0.9; //probability of success on each trial

// Mating parameters
const double theta               = 0.25; //parameters controlling the relationship between rivals waving and probability of mating
const double b                    = 1.0; 
const double pFemMax             = 1.00;
const double pFemMin             = 0.01;
const int multi                    = 20;

//Two morphs parameters
const double q                   = 1.0;
const double alpha              = 0.5;
const double zeta               = 0.5;

/* // pFemMax parameters
const double intercept           = 0.1;
const double slp                   = 1;
const double slp2                = 0.8;
const double slp3               = -1.3; */

// Strategy and updating parameters
const double error              = 0.07; //McNamara error function steepness
const double lam                 = 0.4; //how much the strategy updates by
const int countMax            = 200000; //maximum number of iterations allowed
int counter                        = 0; //loop counter
double largeMaxDiffStrat         = 1.0; //set the starting difference in strategy to be large to get the loop going
double smallMaxDiffStrat         = 1.0;
double largeMaxDiffFit           = 1.0; //set the starting difference in fitness to be large to get the loop going
double smallMaxDiffFit           = 1.0; //set the starting difference in fitness to be large to get the loop going
std::stringstream              outfile; //outputting files

// Lists
double binom                        [n+1]; //probability of kth reward from binomial dist
double HTCprob[3]     = {0.25, 0.5, 0.25}; //probability of getting each high tide cost
double metCostProb[3] = {0.25, 0.5, 0.25}; //probability of getting each metabolic cost
int biggestCell[2]; //looks for the biggest difference between two strategies

bool sim                           = true;

//Simulation paramters
int N                              = 1000;
int simTides                      = tides;

int main()
{
    //Start the loop timer
    auto start = std::chrono::system_clock::now();

    //Initialise multi-dimensional arrays

    //Rt - holds the amount of rivals waving in each timestep
    double **phiLargeWav = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        phiLargeWav[x] = new double [tSteps];
    }

    //Rt - holds the amount of rivals waving in each timestep
    double **phiSmallWav = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        phiSmallWav[x] = new double [tSteps];
    }

    //largePMate - holds the probability of mating in each timestep
    double **largePMate = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        largePMate[x] = new double [tSteps];
    }

    //smallPMate - holds the probability of mating in each timestep
    double **smallPMate = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        smallPMate[x] = new double [tSteps];
    }

    //pFemMaxList - holds the maximum number of females in each timestep
    double **pFemMaxList = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        pFemMaxList[x] = new double [tSteps];
    }

    //pFemMinList - holds the minimum number of females in each timestep
    double **pFemMinList = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        pFemMinList[x] = new double [tSteps];
    }

    double **largePhiDead = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        largePhiDead[x] = new double [tSteps];
    }

    double **smallPhiDead = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        smallPhiDead[x] = new double [tSteps];
    }

    double **largePhiMate = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        largePhiMate[x] = new double [tSteps];
    }

    double **smallPhiMate = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        smallPhiMate[x] = new double [tSteps];
    }

    //Large resident strategy array - holds the resident strategy for each energy level and timestep
    double ***largeResiStrategy = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        largeResiStrategy[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            largeResiStrategy[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    
    //Resident strategy array - holds the resident strategy for each energy level and timestep
    double ***smallResiStrategy = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        smallResiStrategy[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            smallResiStrategy[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response strategy array - holds the best response strategy for each energy level and timestep
    double ***largeBRstrategy = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        largeBRstrategy[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            largeBRstrategy[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response strategy array - holds the best response strategy for each energy level and timestep
    double ***smallBRstrategy = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        smallBRstrategy[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            smallBRstrategy[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response timeout fitness array - holds the fitness of those in timeout in the best response strategy for each energy level and timestep
    double ***largeTimeoutFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        largeTimeoutFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            largeTimeoutFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response timeout fitness array - holds the fitness of those in timeout in the best response strategy for each energy level and timestep
    double ***smallTimeoutFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        smallTimeoutFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            smallTimeoutFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Resident timeout fitness array - holds the fitness of those in timeout in the resident strategy for each energy level and timestep
    double ***largeResiTimeoutFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        largeResiTimeoutFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            largeResiTimeoutFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Resident timeout fitness array - holds the fitness of those in timeout in the resident strategy for each energy level and timestep
    double ***smallResiTimeoutFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        smallResiTimeoutFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            smallResiTimeoutFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response waving fitness array - holds the fitness of waving in the best response strategy for each energy level and timestep
    double ***largeWaveFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        largeWaveFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            largeWaveFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response waving fitness array - holds the fitness of waving in the best response strategy for each energy level and timestep
    double ***smallWaveFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        smallWaveFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            smallWaveFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response foraging fitness array - holds the fitness of foraging in the best response strategy for each energy level and timestep
    double ***largeForFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        largeForFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            largeForFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response foraging fitness array - holds the fitness of foraging in the best response strategy for each energy level and timestep
    double ***smallForFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        smallForFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            smallForFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Resident waving fitness array - holds the fitness of waving in the resident strategy for each energy level and timestep
    double ***largeResiWaveFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        largeResiWaveFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            largeResiWaveFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Resident waving fitness array - holds the fitness of waving in the resident strategy for each energy level and timestep
    double ***smallResiWaveFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        smallResiWaveFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            smallResiWaveFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Resident foraging fitness array - holds the fitness of foraging in the resident strategy for each energy level and timestep
    double ***largeResiForFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        largeResiForFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            largeResiForFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Resident foraging fitness array - holds the fitness of foraging in the resident strategy for each energy level and timestep
    double ***smallResiForFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        smallResiForFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            smallResiForFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response fitness array - holds the fitness of using the best response strategy for each energy level and timestep
    double ***largeBRfitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        largeBRfitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            largeBRfitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response fitness array - holds the fitness of using the best response strategy for each energy level and timestep
    double ***smallBRfitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        smallBRfitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            smallBRfitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Resident strategy fitness array - holds the fitness of using the resident strategy for each energy level and timestep
    double ***largeResiFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        largeResiFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            largeResiFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Resident strategy fitness array - holds the fitness of using the resident strategy for each energy level and timestep
    double ***smallResiFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        smallResiFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            smallResiFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Low tide frequency distribution - holds the proportion of the population that is at each energy level and timestep, in both timeout and non
    double ****largeFreqDist = new double ***[2];
    for(int i=0; i<2; i++)
    {
        largeFreqDist[i] = new double **[tides];
        for(int x=0; x<tides; x++)
        {
            largeFreqDist[i][x] = new double *[eMax];   //create a pointer that is eMax units long
            for(int e=0; e<eMax; e++) //in each of those indices
            {
                largeFreqDist[i][x][e] = new double[tSteps]; //make a pointer that is tSteps units long
            }
        }
    }

    double ****smallFreqDist = new double ***[2];
    for(int i=0; i<2; i++)
    {
        smallFreqDist[i] = new double **[tides];
        for(int x=0; x<tides; x++)
        {
            smallFreqDist[i][x] = new double *[eMax];   //create a pointer that is eMax units long
            for(int e=0; e<eMax; e++) //in each of those indices
            {
                smallFreqDist[i][x][e] = new double[tSteps]; //make a pointer that is tSteps units long
            }
        }
    }

    for(int tide=0; tide<tides; tide++)
    {
        for(int t=0; t<tSteps; t++)
        {
            for(int e=0; e<eMax; e++)
            {
                largeBRstrategy[tide][e][t] = 0.0;
                smallBRstrategy[tide][e][t] = 0.0;

                largeResiStrategy[tide][e][t] = 0.0;
                smallResiStrategy[tide][e][t] = 0.0;
            }
        }
    }

    //Populate binomList with appropriate probabilities
    double sum = 0;
    for(int k=0; k <= n; k++)
    {
        int binomCo = Fact(n)/(Fact(k)*Fact(n-k));
        double payoffProb = pow(p,k) * pow((1-p),(n-k));
        binom[k] = binomCo * payoffProb;
        sum += binom[k];
    }

    //Populate pFemMaxList & pFemMinList
    for(int tide=0; tide<tides; tide++)
    {
        for(int t=0; t<tSteps; t++)
        {
            pFemMaxList[tide][t] = pFemMax;
            pFemMinList[tide][t] = pFemMin; 
        }
    }

    //START OF WHILE LOOP

    while(largeMaxDiffFit > 0.000001 && counter < countMax)
    {
        for(int tide=0; tide<tides; tide++)
        {
            for(int t=0; t<tSteps; t++) //Empty the pMate arrays
            {
                largePMate[tide][t] = 0.0;
                smallPMate[tide][t] = 0.0;

                phiLargeWav[tide][t] = 0.0;
                phiSmallWav[tide][t] = 0.0;

                for(int e=0; e<eMax; e++)
                {
                    if(t==(tSteps-1) & e>0 & tide==(tides-1)) //set the terminal fitness reward function
                    {
                        largeTimeoutFitness[tide][e][t] = 1.0;
                        largeWaveFitness[tide][e][t] = 1.0;
                        largeForFitness[tide][e][t] = 1.0;
                        largeBRfitness[tide][e][t] = 1.0;
                        largeResiFitness[tide][e][t] = 1.0;
                        largeResiTimeoutFitness[tide][e][t] = 1.0;

                        smallTimeoutFitness[tide][e][t] = 1.0;
                        smallWaveFitness[tide][e][t] = 1.0;
                        smallForFitness[tide][e][t] = 1.0;
                        smallBRfitness[tide][e][t] = 1.0;
                        smallResiFitness[tide][e][t] = 1.0;
                        smallResiTimeoutFitness[tide][e][t] = 1.0;
                    }else
                    {
                        largeTimeoutFitness[tide][e][t] = 0.0;
                        largeWaveFitness[tide][e][t] = 0.0;
                        largeForFitness[tide][e][t] = 0.0;
                        largeBRfitness[tide][e][t] = 0.0;
                        largeResiFitness[tide][e][t] = 0.0;
                        largeResiTimeoutFitness[tide][e][t] = 0.0;

                        smallTimeoutFitness[tide][e][t] = 0.0;
                        smallWaveFitness[tide][e][t] = 0.0;
                        smallForFitness[tide][e][t] = 0.0;
                        smallBRfitness[tide][e][t] = 0.0;
                        smallResiFitness[tide][e][t] = 0.0;
                        smallResiTimeoutFitness[tide][e][t] = 0.0;
                    }

                    for(int i=0; i<2; i++) //Empty the frequency distributions (both time in and out)
                    {
                        largeFreqDist[i][tide][e][t] = 0.0;
                        smallFreqDist[i][tide][e][t] = 0.0;
                    }
                }
            }
        }

        //If we're at the very start, set the arbitrary starting strategy
        if(counter==0)
        {
            largeResiStrategy = StartingStrat(largeResiStrategy, tides, eMax, tSteps);
            smallResiStrategy = StartingStrat(smallResiStrategy, tides, eMax, tSteps);
        }

        //Call the Markov function to fill out the population frequency distributions
        ArrayContainer *markovOutput =  Markov(counter,
                                                tides,
                                                eMax, 
                                                tSteps, 
                                                HTcost,
                                                metCostProb,
                                                waveCost,
                                                n,
                                                stochasticHTcost,
                                                b,
                                                multi, 
                                                theta, 
                                                ptau,
                                                postMatingTimeout,     
                                                binom,
                                                phiLargeWav,
                                                phiSmallWav,
                                                largePMate,
                                                smallPMate,
                                                pFemMaxList, 
                                                pFemMinList,
                                                HTCprob,
                                                largeResiStrategy, 
                                                smallResiStrategy,
                                                largeFreqDist,
                                                smallFreqDist,
                                                q,
                                                alpha,
                                                zeta,
                                                pMort, 
                                                pWaveMort);

        //Save the output of the above function
        largeFreqDist = markovOutput->array1;
        largePMate = markovOutput->array2;
        phiLargeWav = markovOutput->array3;

        smallFreqDist = markovOutput->array4;
        smallPMate = markovOutput->array5;
        phiSmallWav = markovOutput->array6;

        //Work backwards through the low tide period, cycling through each energy level per time step
        for(int tide=(tides-1); tide>=0; tide--)
        {
            int tStart;
            if(tide == (tides-1))
            {
                tStart = tSteps-2;
            }
            else
            {
                tStart = tSteps-1;
            }
            for(int t=tStart; t>=0; t--)
            {
                for(int e=1; e<eMax; e++)
                {
                    if(t == tSteps-1 & tide != tides-1) //if in the final timestep of any but the last tide
                    {
                        largeBRfitness[tide][e][t] = HighTide(tides, tide, e, largeBRfitness, HTCprob, HTcost, stochasticHTcost);
                        smallBRfitness[tide][e][t] = HighTide(tides, tide, e, smallBRfitness, HTCprob, HTcost, stochasticHTcost);

                        largeResiFitness[tide][e][t] = HighTide(tides, tide, e, largeResiFitness, HTCprob, HTcost, stochasticHTcost);
                        smallResiFitness[tide][e][t] = HighTide(tides, tide, e, smallResiFitness, HTCprob, HTcost, stochasticHTcost);

                        if(postMatingTimeout == true)
                        {
                            largeTimeoutFitness[tide][e][t] = HighTide(tides, tide, e, largeTimeoutFitness, HTCprob, HTcost, stochasticHTcost);
                            smallTimeoutFitness[tide][e][t] = HighTide(tides, tide, e, smallTimeoutFitness, HTCprob, HTcost, stochasticHTcost);

                            largeResiTimeoutFitness[tide][e][t] = HighTide(tides, tide, e, largeResiTimeoutFitness, HTCprob, HTcost, stochasticHTcost);
                            smallResiTimeoutFitness[tide][e][t] = HighTide(tides, tide, e, smallResiTimeoutFitness, HTCprob, HTcost, stochasticHTcost);
                        }
                    }
                    else //if we're not in the final timestep of a tide
                    {
                        if(postMatingTimeout == true)
                        {
                            //std::cout << "postMatingTimeout fitness for other timesteps\n";
                            largeTimeoutFitness[tide][e][t] = TimeoutFitness(tide, t, e, metCostProb, largeTimeoutFitness, largeBRfitness, ptau, pMort);
                            smallTimeoutFitness[tide][e][t] = TimeoutFitness(tide, t, e, metCostProb, smallTimeoutFitness, smallBRfitness, ptau, pMort);

                            largeResiTimeoutFitness[tide][e][t] = TimeoutFitness(tide, t, e, metCostProb, largeResiTimeoutFitness, largeResiFitness, ptau, pMort);
                            smallResiTimeoutFitness[tide][e][t] = TimeoutFitness(tide, t, e, metCostProb, smallResiTimeoutFitness, smallResiFitness, ptau, pMort);

                        }
                        //std::cout << "BEFOREsmallWaveFitness[" << tide << "][" << e << "][" << t << "] = " << smallWaveFitness[tide][e][t] << "\n";
                        largeWaveFitness[tide][e][t] = WaveFitness(tide, t, e, largeTimeoutFitness, largeBRfitness, waveCost, metCostProb, largePMate, mateBonus, pMort, pWaveMort, postMatingTimeout);
                        smallWaveFitness[tide][e][t] = WaveFitness(tide, t, e, smallTimeoutFitness, smallBRfitness, waveCost, metCostProb, smallPMate, mateBonus, pMort, pWaveMort, postMatingTimeout);

                        largeForFitness[tide][e][t] = ForageFitness(tide, t, e, eMax, largeBRfitness, n, binom, pMort, metCostProb);
                        smallForFitness[tide][e][t] = ForageFitness(tide, t, e, eMax, smallBRfitness, n, binom, pMort, metCostProb);

                        largeResiWaveFitness[tide][e][t] = WaveFitness(tide, t, e, largeResiTimeoutFitness, largeResiFitness, waveCost, metCostProb, largePMate, mateBonus, pMort, pWaveMort, postMatingTimeout);
                        smallResiWaveFitness[tide][e][t] = WaveFitness(tide, t, e, smallResiTimeoutFitness, smallResiFitness, waveCost, metCostProb, smallPMate, mateBonus, pMort, pWaveMort, postMatingTimeout);

                        largeResiForFitness[tide][e][t] = ForageFitness(tide, t, e, eMax, largeResiFitness, n, binom, pMort, metCostProb);
                        smallResiForFitness[tide][e][t] = ForageFitness(tide, t, e, eMax, smallResiFitness, n, binom, pMort, metCostProb);


                        double largeFitDiff = largeForFitness[tide][e][t] - largeWaveFitness[tide][e][t];
                        double largeFitDiffErr = largeFitDiff/error;
                        double largeExponent = exp(largeFitDiffErr);
                        double largeExponentPlusOne = largeExponent + 1;
                        largeBRstrategy[tide][e][t] = 1/largeExponentPlusOne;

                        double smallFitDiff = smallForFitness[tide][e][t] - smallWaveFitness[tide][e][t];
                        double smallFitDiffErr = smallFitDiff/error;
                        double smallExponent = exp(smallFitDiffErr);
                        double smallExponentPlusOne = smallExponent + 1;
                        smallBRstrategy[tide][e][t] = 1/smallExponentPlusOne;

                        largeBRfitness[tide][e][t] = (largeBRstrategy[tide][e][t] * largeWaveFitness[tide][e][t]) + ((1-largeBRstrategy[tide][e][t]) * largeForFitness[tide][e][t]);
                        smallBRfitness[tide][e][t] = (smallBRstrategy[tide][e][t] * smallWaveFitness[tide][e][t]) + ((1-smallBRstrategy[tide][e][t]) * smallForFitness[tide][e][t]);

                        largeResiFitness[tide][e][t] = (largeResiStrategy[tide][e][t] * largeResiWaveFitness[tide][e][t]) + ((1-largeResiStrategy[tide][e][t]) * largeResiForFitness[tide][e][t]);
                        smallResiFitness[tide][e][t] = (smallResiStrategy[tide][e][t] * smallResiWaveFitness[tide][e][t]) + ((1-smallResiStrategy[tide][e][t]) * smallResiForFitness[tide][e][t]);

                    }
                }
            }
        }
        largeMaxDiffStrat = 0.0;
        smallMaxDiffStrat = 0.0;

        largeMaxDiffFit = 0.0;
        smallMaxDiffFit = 0.0;

        for(int tide=0; tide<tides; tide++)
        {
            for(int t=0; t<(tSteps-1); t++)
            {
                for(int e=1; e<eMax; e++)
                {
                    double largeDiffStrat = abs(largeBRstrategy[tide][e][t] - largeResiStrategy[tide][e][t]);
                    double smallDiffStrat = abs(smallBRstrategy[tide][e][t] - smallResiStrategy[tide][e][t]);

                    double largeDiffFit = abs(largeBRfitness[tide][e][t] - largeResiFitness[tide][e][t]);
                    double smallDiffFit = abs(smallBRfitness[tide][e][t] - smallResiFitness[tide][e][t]);


                    if(largeDiffStrat > largeMaxDiffStrat)
                    {
                        largeMaxDiffStrat = largeDiffStrat;   
                    }
                    if(smallDiffStrat > smallMaxDiffStrat)
                    {
                        smallMaxDiffStrat = smallDiffStrat;   
                    }

                    if(largeDiffFit > largeMaxDiffFit)
                    {
                        largeMaxDiffFit = largeDiffFit;
                    }
                    if(smallDiffFit > smallMaxDiffFit)
                    {
                        smallMaxDiffFit = smallDiffFit;
                    }
                }
            }
        }
        //std::cout << "smallResiStrategy[2][1][0] = " << smallResiStrategy[2][1][0] << "\n";
        for(int tide=0; tide<tides; tide++)
        {     
            for(int t=0; t<tSteps; t++)
            {
                for(int e=0; e<eMax; e++)
                {


                    largeResiStrategy[tide][e][t] = (lam * largeBRstrategy[tide][e][t]) + ((1-lam) * largeResiStrategy[tide][e][t]);
                    smallResiStrategy[tide][e][t] = (lam * smallBRstrategy[tide][e][t]) + ((1-lam) * smallResiStrategy[tide][e][t]);
                }
            }
        }
        counter++;

        if(counter % 10 == 0){
            auto timeNow = std::chrono::system_clock::now();

            std::cout << "counter = " << counter << ", largeMaxDiffFit = " << largeMaxDiffFit << "\n";
            std::cout << "smallMaxDiffFit = " << smallMaxDiffFit << "\n";

            std::chrono::duration<double> elapsed_seconds = timeNow-start;
            std::time_t end_time = std::chrono::system_clock::to_time_t(timeNow);
            std::cout << "elapsed time = " << (elapsed_seconds.count())/60 <<"mins \n\n";
        }
    } //end of while loop
    auto timeNow = std::chrono::system_clock::now();
    std::cout << " final counter = " << counter << ", final largeMaxDiffFit = " << largeMaxDiffFit << "\n";
    std::cout << " final smallMaxDiffFit = " << largeMaxDiffFit << "\n";
    std::chrono::duration<double> elapsed_seconds = timeNow-start;
    std::time_t end_time = std::chrono::system_clock::to_time_t(timeNow);
    std::cout << " total elapsed time = " << (elapsed_seconds.count())/60 <<"mins \n\n";

    for(int i=0; i<2; i++)
    {
        for(int tide=0; tide<tides; tide++)
        {
            for(int e=0; e<eMax; e++)
            {
                for(int t=0; t<tSteps; t++)
                {
                    largeFreqDist[i][tide][e][t] = 0.0;
                    smallFreqDist[i][tide][e][t] = 0.0;
                }
            }
        }
    }



    ArrayContainer *markovOutput2 =  Markov(counter,
                                                tides,
                                                eMax, 
                                                tSteps, 
                                                HTcost,
                                                metCostProb,
                                                waveCost,
                                                n,
                                                stochasticHTcost,
                                                b, 
                                                multi,
                                                theta, 
                                                ptau,
                                                postMatingTimeout,     
                                                binom,
                                                phiLargeWav,
                                                phiSmallWav,
                                                largePMate,
                                                smallPMate,
                                                pFemMaxList, 
                                                pFemMinList,
                                                HTCprob,
                                                largeResiStrategy, 
                                                smallResiStrategy,
                                                largeFreqDist,
                                                smallFreqDist,
                                                q,
                                                alpha,
                                                zeta,
                                                pMort, 
                                                pWaveMort);

    largeFreqDist = markovOutput2->array1;
    largePMate = markovOutput2->array2;
    phiLargeWav = markovOutput2->array3;

    smallFreqDist = markovOutput2->array4;
    smallPMate = markovOutput2->array5;
    phiSmallWav = markovOutput2->array6;

    std::ofstream largeBRstratOut;
    std::ostringstream largeBRstratOutFilename;
    largeBRstratOutFilename << "output_files/largeBRstrat.txt"; 
    largeBRstratOut.open (largeBRstratOutFilename.str());
    for(int e=0; e<eMax; e++)
    {
        largeBRstratOut << largeBRstrategy[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                largeBRstratOut << "," << largeBRstrategy[tide][e][t];
            }
            largeBRstratOut << "," << "HT";
        }
        largeBRstratOut << std::endl;
    }
    largeBRstratOut.close();

    std::ofstream largeResiStratOut;
    std::ostringstream largeResiStratOutFilename;
    largeResiStratOutFilename << "output_files/largeResiStrat.txt"; 
    largeResiStratOut.open (largeResiStratOutFilename.str());
    for(int e=0; e<eMax; e++)
    {
        largeResiStratOut << largeResiStrategy[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                largeResiStratOut << "," << largeResiStrategy[tide][e][t];
            }
            largeResiStratOut << "," << "HT";
        }
        largeResiStratOut << std::endl;
    }
    largeResiStratOut.close();


    std::ofstream largeFreqDistOut;
    outfile.str("output_files/largeFreqDist.txt");
    std::string largeFreqDistOutFilename = outfile.str();
    largeFreqDistOut.open (largeFreqDistOutFilename.c_str());
        for(int e=0; e<eMax; e++)
        {
            largeFreqDistOut << largeFreqDist[0][0][e][0];
            for(int tide=0; tide<tides; tide++)
            {
                int tStart;
                if(tide == 0)
                {
                    tStart = 1;
                }
                else
                {
                    tStart = 0;
                }
                for(int t=tStart; t<tSteps; t++)
                {
                    largeFreqDistOut << "," << largeFreqDist[0][tide][e][t];
                }
                largeFreqDistOut << "," << "HT";
            }
            largeFreqDistOut << std::endl;
        }
    largeFreqDistOut.close(); 

    std::ofstream largeFreqDistTimeout;
    outfile.str("output_files/largeFreqDistTimeout.txt");
    std::string largeFreqDistTimeoutFilename = outfile.str();
    largeFreqDistTimeout.open (largeFreqDistTimeoutFilename.c_str());
        for(int e=0; e<eMax; e++)
        {
            largeFreqDistTimeout << largeFreqDist[1][0][e][0];
            for(int tide=0; tide<tides; tide++)
            {
                int tStart;
                if(tide == 0)
                {
                    tStart = 1;
                }
                else
                {
                    tStart = 0;
                }
                for(int t=tStart; t<tSteps; t++)
                {
                    largeFreqDistTimeout << "," << largeFreqDist[1][tide][e][t];
                }
                largeFreqDistTimeout << "," << "HT";
            }
            largeFreqDistTimeout << std::endl;
        }
    largeFreqDistTimeout.close(); 

    std::ofstream largeWaveFit;
    outfile.str("output_files/largeWaveFit.txt");
    std::string largeWaveFitFilename = outfile.str();
    largeWaveFit.open (largeWaveFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        largeWaveFit << largeWaveFitness[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                largeWaveFit << "," << largeWaveFitness[tide][e][t];
            }
            largeWaveFit << "," << "HT";
        }
        largeWaveFit << std::endl;
    }
    largeWaveFit.close(); 

    std::ofstream largeForFit;
    outfile.str("output_files/largeForFit.txt");
    std::string largeForFitFilename = outfile.str();
    largeForFit.open (largeForFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        largeForFit << largeForFitness[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                largeForFit << "," << largeForFitness[tide][e][t];
            }
            largeForFit << "," << "HT";
        }
        largeForFit << std::endl;
    }   
    largeForFit.close();

    std::ofstream largeBRFit;
    outfile.str("output_files/largeBRFit.txt");
    std::string largeBRFitFilename = outfile.str();
    largeBRFit.open (largeBRFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        largeBRFit << largeBRfitness[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                largeBRFit << "," << largeBRfitness[tide][e][t];
            }
            largeBRFit << "," << "HT";
        }
        largeBRFit << std::endl;
    }
    largeBRFit.close(); 

    std::ofstream largeResiFit;
    outfile.str("output_files/largeResiFit.txt");
    std::string largeResiFitFilename = outfile.str();
    largeResiFit.open (largeResiFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        largeResiFit << largeResiFitness[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                largeResiFit << "," << largeResiFitness[tide][e][t];
            }
            largeResiFit << "," << "HT";
        }
        largeResiFit << std::endl;
    }
    largeResiFit.close(); 

    std::ofstream largeTimeoutFit;
    outfile.str("output_files/largeTimeoutFit.txt");
    std::string largeTimeoutFitFilename = outfile.str();
    largeTimeoutFit.open (largeTimeoutFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        largeTimeoutFit << largeTimeoutFitness[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                largeTimeoutFit << "," << largeTimeoutFitness[tide][e][t];
            }
            largeTimeoutFit << "," << "HT";
        }
        largeTimeoutFit << std::endl;
    }
    largeTimeoutFit.close(); 

    std::ofstream largePMateOut;
    outfile.str("output_files/largeFinalpMate.txt");
    std::string largePMateFilename = outfile.str();
    largePMateOut.open(largePMateFilename.c_str());
    largePMateOut << largePMate[0][0] << ",";
    for(int tide=0; tide<tides; tide++)
    {
        int tStart;
        if(tide == 0)
        {
            tStart = 1;
        }
        else
        {
            tStart = 0;
        }
        for(int t=tStart; t<tSteps; t++)
        {
        largePMateOut << largePMate[tide][t] << ",";
        }
        largePMateOut << "," << "HT" << ",";
    }
    largePMateOut.close();

    std::ofstream phiLargeWavOut;
    outfile.str("output_files/phiLargeWav.txt");
    std::string phiLargeWavFilename = outfile.str();
    phiLargeWavOut.open(phiLargeWavFilename.c_str());
    phiLargeWavOut << phiLargeWav[0][0] << ",";
    for(int tide=0; tide<tides; tide++)
    {
        int tStart;
        if(tide == 0)
        {
            tStart = 1;
        }
        else
        {
            tStart = 0;
        }
        for(int t=tStart; t<tSteps; t++)
        {
            phiLargeWavOut << phiLargeWav[tide][t] << ",";
        }
        phiLargeWavOut << "," << "HT" << ",";
    }
    phiLargeWavOut.close();


    for(int tide = 0; tide < tides; tide++)
    {
        for(int t = 0; t< tSteps; t++)
        {
            largePhiDead[tide][t] = largeFreqDist[0][tide][0][t];

            if(postMatingTimeout == true)
            {
                largePhiDead[tide][t] += largeFreqDist[1][tide][0][t];
            }
        }
    }

    std::ofstream largePhiDeadOut;
    outfile.str("output_files/largePhiDead.txt");
    std::string largePhiDeadFilename = outfile.str();
    largePhiDeadOut.open(largePhiDeadFilename.c_str());
    largePhiDeadOut << largePhiDead[0][0] << ",";
    for(int tide=0; tide<tides; tide++)
    {
        int tStart;
        if(tide == 0)
        {
            tStart = 1;
        }
        else
        {
            tStart = 0;
        }
        for(int t=tStart; t<tSteps; t++)
        {
            largePhiDeadOut << largePhiDead[tide][t] << ",";
        }
        largePhiDeadOut << "," << "HT" << ",";
    }
    largePhiDeadOut.close();

    double propLargeMating;
    for(int tide = 0; tide < tides; tide++)
    {
        for(int t = 0; t < tSteps; t++)
        {
            propLargeMating = 0;
            for(int e = 0; e < eMax; e++)
            {
                propLargeMating += largeFreqDist[0][tide][e][t] * largeBRstrategy[tide][e][t] * largePMate[tide][t];
            }
            largePhiMate[tide][t] = propLargeMating;
        }
    }

    std::ofstream largePhiMateOut;
    outfile.str("output_files/largePhiMate.txt");
    std::string largePhiMateFilename = outfile.str();
    largePhiMateOut.open(largePhiMateFilename.c_str());
    largePhiMateOut << largePhiMate[0][0] << ",";
    for(int tide=0; tide<tides; tide++)
    {
        int tStart;
        if(tide == 0)
        {
            tStart = 1;
        }
        else
        {
            tStart = 0;
        }
        for(int t=tStart; t<tSteps; t++)
        {
            largePhiMateOut << largePhiMate[tide][t] << ",";
        }
        largePhiMateOut << "," << "HT" << ",";
    }
    largePhiMateOut.close();



    //start of small output
    std::ofstream smallBRstratOut;
    std::ostringstream smallBRstratOutFilename;
    smallBRstratOutFilename << "output_files/smallBRstrat.txt"; 
    smallBRstratOut.open (smallBRstratOutFilename.str());
    for(int e=0; e<eMax; e++)
    {
        smallBRstratOut << smallBRstrategy[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                smallBRstratOut << "," << smallBRstrategy[tide][e][t];
            }
            smallBRstratOut << "," << "HT";
        }
        smallBRstratOut << std::endl;
    }
    smallBRstratOut.close();

    std::ofstream smallResiStratOut;
    std::ostringstream smallResiStratOutFilename;
    smallResiStratOutFilename << "output_files/smallResiStrat.txt"; 
    smallResiStratOut.open (smallResiStratOutFilename.str());
    for(int e=0; e<eMax; e++)
    {
        smallResiStratOut << smallResiStrategy[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                smallResiStratOut << "," << smallResiStrategy[tide][e][t];
            }
            smallResiStratOut << "," << "HT";
        }
        smallResiStratOut << std::endl;
    }
    smallResiStratOut.close();


    std::ofstream smallFreqDistOut;
    outfile.str("output_files/smallFreqDist.txt");
    std::string smallFreqDistOutFilename = outfile.str();
    smallFreqDistOut.open (smallFreqDistOutFilename.c_str());
        for(int e=0; e<eMax; e++)
        {
            smallFreqDistOut << smallFreqDist[0][0][e][0];
            for(int tide=0; tide<tides; tide++)
            {
                int tStart;
                if(tide == 0)
                {
                    tStart = 1;
                }
                else
                {
                    tStart = 0;
                }
                for(int t=tStart; t<tSteps; t++)
                {
                    smallFreqDistOut << "," << smallFreqDist[0][tide][e][t];
                }
                smallFreqDistOut << "," << "HT";
            }
            smallFreqDistOut << std::endl;
        }
    smallFreqDistOut.close(); 
    

    std::ofstream smallFreqDistTimeout;
    outfile.str("output_files/smallFreqDistTimeout.txt");
    std::string smallFreqDistTimeoutFilename = outfile.str();
    smallFreqDistTimeout.open (smallFreqDistTimeoutFilename.c_str());
        for(int e=0; e<eMax; e++)
        {
            smallFreqDistTimeout << smallFreqDist[1][0][e][0];
            for(int tide=0; tide<tides; tide++)
            {
                int tStart;
                if(tide == 0)
                {
                    tStart = 1;
                }
                else
                {
                    tStart = 0;
                }
                for(int t=tStart; t<tSteps; t++)
                {
                    smallFreqDistTimeout << "," << smallFreqDist[1][tide][e][t];
                }
                smallFreqDistTimeout << "," << "HT";
            }
            smallFreqDistTimeout << std::endl;
        }
    smallFreqDistTimeout.close(); 

    std::ofstream smallWaveFit;
    outfile.str("output_files/smallWaveFit.txt");
    std::string smallWaveFitFilename = outfile.str();
    smallWaveFit.open (smallWaveFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        smallWaveFit << smallWaveFitness[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                smallWaveFit << "," << smallWaveFitness[tide][e][t];
            }
            smallWaveFit << "," << "HT";
        }
        smallWaveFit << std::endl;
    }
    smallWaveFit.close(); 

    std::ofstream smallForFit;
    outfile.str("output_files/smallForFit.txt");
    std::string smallForFitFilename = outfile.str();
    smallForFit.open (smallForFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        smallForFit << smallForFitness[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                smallForFit << "," << smallForFitness[tide][e][t];
            }
            smallForFit << "," << "HT";
        }
        smallForFit << std::endl;
    }   
    smallForFit.close();

    std::ofstream smallBRFit;
    outfile.str("output_files/smallBRFit.txt");
    std::string smallBRFitFilename = outfile.str();
    smallBRFit.open (smallBRFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        smallBRFit << smallBRfitness[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                smallBRFit << "," << smallBRfitness[tide][e][t];
            }
            smallBRFit << "," << "HT";
        }
        smallBRFit << std::endl;
    }
    smallBRFit.close(); 

    std::ofstream smallResiFit;
    outfile.str("output_files/smallResiFit.txt");
    std::string smallResiFitFilename = outfile.str();
    smallResiFit.open (smallResiFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        smallResiFit << smallResiFitness[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                smallResiFit << "," << smallResiFitness[tide][e][t];
            }
            smallResiFit << "," << "HT";
        }
        smallResiFit << std::endl;
    }
    smallResiFit.close(); 

    std::ofstream smallTimeoutFit;
    outfile.str("output_files/smallTimeoutFit.txt");
    std::string smallTimeoutFitFilename = outfile.str();
    smallTimeoutFit.open (smallTimeoutFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        smallTimeoutFit << smallTimeoutFitness[0][e][0];
        for(int tide=0; tide<tides; tide++)
        {
            int tStart;
            if(tide == 0)
            {
                tStart = 1;
            }
            else
            {
                tStart = 0;
            }
            for(int t=tStart; t<tSteps; t++)
            {
                smallTimeoutFit << "," << smallTimeoutFitness[tide][e][t];
            }
            smallTimeoutFit << "," << "HT";
        }
        smallTimeoutFit << std::endl;
    }
    smallTimeoutFit.close(); 

    std::ofstream smallPMateOut;
    outfile.str("output_files/smallFinalpMate.txt");
    std::string smallPMateFilename = outfile.str();
    smallPMateOut.open(smallPMateFilename.c_str());
    smallPMateOut << smallPMate[0][0] << ",";
    for(int tide=0; tide<tides; tide++)
    {
        int tStart;
        if(tide == 0)
        {
            tStart = 1;
        }
        else
        {
            tStart = 0;
        }
        for(int t=tStart; t<tSteps; t++)
        {
        smallPMateOut << smallPMate[tide][t] << ",";
        }
        smallPMateOut << "," << "HT" << ",";
    }
    smallPMateOut.close();

    std::ofstream phiSmallWavOut;
    outfile.str("output_files/phismallWav.txt");
    std::string phiSmallWavFilename = outfile.str();
    phiSmallWavOut.open(phiSmallWavFilename.c_str());
    phiSmallWavOut << phiSmallWav[0][0] << ",";
    for(int tide=0; tide<tides; tide++)
    {
        int tStart;
        if(tide == 0)
        {
            tStart = 1;
        }
        else
        {
            tStart = 0;
        }
        for(int t=tStart; t<tSteps; t++)
        {
            phiSmallWavOut << phiSmallWav[tide][t] << ",";
        }
        phiSmallWavOut << "," << "HT" << ",";
    }
    phiSmallWavOut.close();


    for(int tide = 0; tide < tides; tide++)
    {
        for(int t = 0; t< tSteps; t++)
        {
            smallPhiDead[tide][t] = smallFreqDist[0][tide][0][t];

            if(postMatingTimeout == true)
            {
                smallPhiDead[tide][t] += smallFreqDist[1][tide][0][t];
            }
        }
    }

    std::ofstream smallPhiDeadOut;
    outfile.str("output_files/smallPhiDead.txt");
    std::string smallPhiDeadFilename = outfile.str();
    smallPhiDeadOut.open(smallPhiDeadFilename.c_str());
    smallPhiDeadOut << smallPhiDead[0][0] << ",";
    for(int tide=0; tide<tides; tide++)
    {
        int tStart;
        if(tide == 0)
        {
            tStart = 1;
        }
        else
        {
            tStart = 0;
        }
        for(int t=tStart; t<tSteps; t++)
        {
            smallPhiDeadOut << smallPhiDead[tide][t] << ",";
        }
        smallPhiDeadOut << "," << "HT" << ",";
    }
    smallPhiDeadOut.close();

    double propsmallMating;
    for(int tide = 0; tide < tides; tide++)
    {
        for(int t = 0; t < tSteps; t++)
        {
            propsmallMating = 0;
            for(int e = 0; e < eMax; e++)
            {
                propsmallMating += smallFreqDist[0][tide][e][t] * smallBRstrategy[tide][e][t] * smallPMate[tide][t];
            }
            smallPhiMate[tide][t] = propsmallMating;
        }
    }

    std::ofstream smallPhiMateOut;
    outfile.str("output_files/smallPhiMate.txt");
    std::string smallPhiMateFilename = outfile.str();
    smallPhiMateOut.open(smallPhiMateFilename.c_str());
    smallPhiMateOut << smallPhiMate[0][0] << ",";
    for(int tide=0; tide<tides; tide++)
    {
        int tStart;
        if(tide == 0)
        {
            tStart = 1;
        }
        else
        {
            tStart = 0;
        }
        for(int t=tStart; t<tSteps; t++)
        {
            smallPhiMateOut << smallPhiMate[tide][t] << ",";
        }
        smallPhiMateOut << "," << "HT" << ",";
    }
    smallPhiMateOut.close(); 


    if(sim)
    {
        // Create a random device
        std::random_device rd;

        // Initialize a random number generator
        std::default_random_engine engine(rd());

        // Create a uniform_real_distribution between 0 and 1
        std::uniform_real_distribution<double> dist(0.0, 1.0);

        int ***simEnergy = new int **[simTides]; //[tides][individual][timestep]
        for(int x=0; x<simTides; x++)
        {
            simEnergy[x] = new int *[N];   //create a pointer that is eMax units long
            for(int i=0; i<N; i++) //in each of those indices
            {
                simEnergy[x][i] = new int[tSteps]; //make a pointer that is tSteps units long
            }
        }

        double ***simMating = new double **[simTides];
        for(int x=0; x<simTides; x++)
        {
            simMating[x] = new double *[N];   //create a pointer that is eMax units long
            for(int i=0; i<N; i++) //in each of those indices
            {
                simMating[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
            }
        }

        double ***simAlive = new double **[simTides];
        for(int x=0; x<simTides; x++)
        {
            simAlive[x] = new double *[N];   //create a pointer that is eMax units long
            for(int i=0; i<N; i++) //in each of those indices
            {
                simAlive[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
            }
        }

        double ***simTimeout = new double **[simTides];
        for(int x=0; x<simTides; x++)
        {
            simTimeout[x] = new double *[N];   //create a pointer that is eMax units long
            for(int i=0; i<N; i++) //in each of those indices
            {
                simTimeout[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
            }
        }

        char ***simBehav = new char **[simTides];
        for(int x=0; x<simTides; x++)
        {
            simBehav[x] = new char *[N];   //create a pointer that is eMax units long
            for(int i=0; i<N; i++) //in each of those indices
            {
                simBehav[x][i] = new char[tSteps]; //make a pointer that is tSteps units long
            }
        }

        int* simSizes = new int[N];

        for(int z = 0; z < N; z++)
        {
            double random_double = dist(engine);
            if(random_double < q)
            {
                simSizes[z] = 1; //large 

            }
            else
            {
                simSizes[z] = 0; //small
            }
        }

        double **largeSimStrat = new double *[eMax];   //create a pointer that is eMax units long
        double **smallSimStrat = new double *[eMax];
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            largeSimStrat[i] = new double[tSteps]; //make a pointer that is tSteps units long
            smallSimStrat[i] = new double[tSteps]; //make a pointer that is tSteps units long
        }

        double pMateL[tSteps];
        double pMateS[tSteps];

        for(int t = 0; t < tSteps; t ++)
        {
            pMateL[t] = largePMate[tides/2][t];
            pMateS[t] = smallPMate[tides/2][t];
        }

        for(int t = 0; t < tSteps; t ++)
        {
            for(int e = 0; e < eMax; e ++)
            {
                largeSimStrat[e][t] = largeBRstrategy[tides/2][e][t];
                smallSimStrat[e][t] = smallBRstrategy[tides/2][e][t];
            }
        }

        for(int tide = 0; tide < simTides; tide ++)
        {
            for(int z = 0; z < N; z ++)
            {
                for(int t = 0; t < tSteps; t ++)
                {
                    simAlive[tide][z][t] = 1;
                    simMating[tide][z][t] = 0;
                    simTimeout[tide][z][t] = 0;
                }
            }
        }

        ArrayContainer *simOutput =  Sim(N,
                                        eMax, 
                                        tSteps, 
                                        simTides, 
                                        HTcost, 
                                        pMateL,
                                        pMateS, 
                                        waveCost,
                                        n, 
                                        p,
                                        stochasticHTcost,
                                        b, 
                                        theta, 
                                        ptau,
                                        postMatingTimeout,
                                        largeSimStrat,
                                        smallSimStrat,
                                        q,
                                        alpha,
                                        zeta,
                                        pMort,
                                        pWaveMort,
                                        simEnergy,
                                        simMating,
                                        simAlive,
                                        simTimeout,
                                        simBehav,
                                        simSizes);

        simMating = simOutput->array7;
        simTimeout = simOutput->array8;
        simAlive = simOutput->array9;
        simEnergy = simOutput->array10;
        simBehav = simOutput->array11;

        std::cout << "Simulation of " << N << " individuals complete! Have a nice day! \n\n";

        std::ofstream simSizesOut;
        std::ostringstream simSizesOutFilename;
        simSizesOutFilename << "output_files/simSizes.txt"; 
        simSizesOut.open (simSizesOutFilename.str());
        simSizesOut << simSizes[0];
        for(int z=1; z<N; z++)
        {
            simSizesOut << "," << simSizes[z];
        }
        simSizesOut.close();

        std::ofstream simMatingOut;
        std::ostringstream simMatingOutFilename;
        simMatingOutFilename << "output_files/simMating.txt"; 
        simMatingOut.open (simMatingOutFilename.str());
        for(int z=0; z<N; z++)
        {
            simMatingOut << simMating[0][z][0];
            for(int tide=0; tide<simTides; tide++)
            {
                int tStart;
                if(tide == 0)
                {
                    tStart = 1;
                }
                else
                {
                    tStart = 0;
                }
                for(int t=tStart; t<tSteps; t++)
                {
                    simMatingOut << "," << simMating[tide][z][t];
                }
                simMatingOut << "," << "HT";
            }
            simMatingOut << std::endl;
        }
        simMatingOut.close();

        std::ofstream simTimeoutOut;
        std::ostringstream simTimeoutOutFilename;
        simTimeoutOutFilename << "output_files/simTimeout.txt"; 
        simTimeoutOut.open (simTimeoutOutFilename.str());
        for(int z=0; z<N; z++)
        {
            simTimeoutOut << simTimeout[0][z][0];
            for(int tide=0; tide<simTides; tide++)
            {
                int tStart;
                if(tide == 0)
                {
                    tStart = 1;
                }
                else
                {
                    tStart = 0;
                }
                for(int t=tStart; t<tSteps; t++)
                {
                    simTimeoutOut << "," << simTimeout[tide][z][t];
                }
                simTimeoutOut << "," << "HT";
            }
            simTimeoutOut << std::endl;
        }
        simTimeoutOut.close();

        std::ofstream simAliveOut;
        std::ostringstream simAliveOutFilename;
        simAliveOutFilename << "output_files/simAlive.txt"; 
        simAliveOut.open (simAliveOutFilename.str());
        for(int z=0; z<N; z++)
        {
            simAliveOut << simAlive[0][z][0];
            for(int tide=0; tide<simTides; tide++)
            {
                int tStart;
                if(tide == 0)
                {
                    tStart = 1;
                }
                else
                {
                    tStart = 0;
                }
                for(int t=tStart; t<tSteps; t++)
                {
                    simAliveOut << "," << simAlive[tide][z][t];
                }
            simAliveOut << "," << "HT";
            }
            simAliveOut << std::endl;
        }
        simAliveOut.close();

        std::ofstream simEnergyOut;
        std::ostringstream simEnergyOutFilename;
        simEnergyOutFilename << "output_files/simEnergy.txt"; 
        simEnergyOut.open (simEnergyOutFilename.str());
        for(int z=0; z<N; z++)
        {
            simEnergyOut << simEnergy[0][z][0];
            for(int tide=0; tide<simTides; tide++)
            {
                int tStart;
                if(tide == 0)
                {
                    tStart = 1;
                }
                else
                {
                    tStart = 0;
                }
                for(int t=tStart; t<tSteps; t++)
                {
                    simEnergyOut << "," << simEnergy[tide][z][t];
                }
            simEnergyOut << "," << "HT";
            }
            simEnergyOut << std::endl;
        }
        simEnergyOut.close();

        std::ofstream simBehavOut;
        std::ostringstream simBehavOutFilename;
        simBehavOutFilename << "output_files/simBehav.txt"; 
        simBehavOut.open (simBehavOutFilename.str());
        for(int z=0; z<N; z++)
        {
            simBehavOut << simBehav[0][z][0];
            for(int tide=0; tide<simTides; tide++)
            {
                int tStart;
                if(tide == 0)
                {
                    tStart = 1;
                }
                else
                {
                    tStart = 0;
                }
                for(int t=tStart; t<tSteps; t++)
                {
                    simBehavOut << "," << simBehav[tide][z][t];
                }
            simBehavOut << "," << "HT";
            }
            simBehavOut << std::endl;
        }
        simBehavOut.close();
    }

}