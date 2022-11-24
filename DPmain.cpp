//Header files
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <math.h> 
#include <bits/stdc++.h>
#include <chrono>
#include <ctime>   
#include "functions.h"

// Initialising...
// Array dimensions
const int eMax                   = 51; //energy levels
const int tSteps                 = 50; //timeSteps
const int tides                  = 30; //tides (fixed number)

// High tide cost parameters
const int HTcost                  = 10; //energetic cost of high tide
const bool stochasticHTcost     = true; //is the high tide cost fixed or stochastic

// Costs
const int waveCost                 = 5; //cost of waving
const double ptau                = 0.2; //probability of returning to non-timeout array
const bool postMatingTimeout    = true;
const int mateBonus                = 5; //fitness bonus from mating
const double pMort           = 0.00001; //probability of death in every timeStep
const double pWaveMort             = 2; //multiplier for how much more mortality occurs when you wave

// Foraging gains parameters
const int n                        = 4; //max energy gain from foraging
const double p                   = 0.9; //probability of success on each trial

// Mating parameters
const double alpha               = 0.3; //parameters controlling the relationship between rivals waving and probability of mating
const double b                   = 0.1; //same as above
const double pFemMax            = 0.05;
const double pFemMin          = 0.0001;

/* // pFemMax parameters
const double intercept           = 0.1;
const double slp                   = 1;
const double slp2                = 0.8;
const double slp3               = -1.3; */

// Strategy and updating parameters
const double error               = 0.07; //McNamara error function steepness
const double lam                 = 0.4; //how much the strategy updates by
const double countMax         = 200000; //maximum number of iterations allowed
int counter                        = 0; //loop counter
double maxDiffStrat              = 1.0; //set the starting difference in strategy to be large to get the loop going
double maxDiffFit                = 1.0; //set the starting difference in fitness to be large to get the loop going
std::stringstream              outfile; //outputting files

// Lists
double binom                        [n+1]; //probability of kth reward from binomial dist
double HTCprob[3]     = {0.25, 0.5, 0.25}; //probability of getting each high tide cost
double metCostProb[3] = {0.25, 0.5, 0.25}; //probability of getting each metabolic cost
int biggestCell[2]; //looks for the biggest difference between two strategies

/* const int spreadOfDead = 3;
double deadDist[spreadOfDead] = {0.25, 0.5, 0.25};
int lowestDeadSpread = 0;  */

/* const int spreadOfDead = 31;
double deadDist[spreadOfDead];
const double deadMu = 25;
const double deadSD = 15;
const int lowestDeadSpread = deadMu - ((spreadOfDead-1)/2); */


int main()
{
    //Start the loop timer
    auto start = std::chrono::system_clock::now();

    //Initialise multi-dimensional arrays

    //Rt - holds the amount of rivals waving in each timestep
    double **Rt = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        Rt[x] = new double [tSteps];
    }

    //pMate - holds the probability of mating in each timestep
    double **pMate = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        pMate[x] = new double [tSteps];
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

    double **phiDead = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        phiDead[x] = new double [tSteps];
    }

    double **phiMate = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        phiMate[x] = new double [tSteps];
    }

    //Resident strategy array - holds the resident strategy for each energy level and timestep
    double ***resiStrategy = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        resiStrategy[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            resiStrategy[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response strategy array - holds the best response strategy for each energy level and timestep
    double ***BRstrategy = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        BRstrategy[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            BRstrategy[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response timeout fitness array - holds the fitness of those in timeout in the best response strategy for each energy level and timestep
    double ***timeoutFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        timeoutFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            timeoutFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }


    //Resident timeout fitness array - holds the fitness of those in timeout in the resident strategy for each energy level and timestep
    double ***resiTimeoutFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        resiTimeoutFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            resiTimeoutFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response waving fitness array - holds the fitness of waving in the best response strategy for each energy level and timestep
    double ***waveFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        waveFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            waveFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response foraging fitness array - holds the fitness of foraging in the best response strategy for each energy level and timestep
    double ***forFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        forFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            forFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Resident waving fitness array - holds the fitness of waving in the resident strategy for each energy level and timestep
    double ***resiWaveFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        resiWaveFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            resiWaveFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Resident foraging fitness array - holds the fitness of foraging in the resident strategy for each energy level and timestep
    double ***resiForFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        resiForFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            resiForFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Best response fitness array - holds the fitness of using the best response strategy for each energy level and timestep
    double ***BRfitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        BRfitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            BRfitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Resident strategy fitness array - holds the fitness of using the resident strategy for each energy level and timestep
    double ***resiFitness = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        resiFitness[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            resiFitness[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    //Low tide frequency distribution - holds the proportion of the population that is at each energy level and timestep, in both timeout and non
    double ****freqDist = new double ***[2];
    for(int i=0; i<2; i++)
    {
        freqDist[i] = new double **[tides];
        for(int x=0; x<tides; x++)
        {
            freqDist[i][x] = new double *[eMax];   //create a pointer that is eMax units long
            for(int e=0; e<eMax; e++) //in each of those indices
            {
                freqDist[i][x][e] = new double[tSteps]; //make a pointer that is tSteps units long
            }
        }
    }

    //Set the terminal fitness reward and set the rest as zero
    for(int tide=0; tide<tides; tide++)
    {
        for(int t=0; t<tSteps; t++)
        {
            for(int e=0; e<eMax; e++)
            {
                if(t==(tSteps-1) & e>0 & tide==(tides-1))
                {          
                    BRstrategy[tide][e][t] = 0.0;
                    resiStrategy[tide][e][t] = 0.0;
                }
                else
                {
                    BRstrategy[tide][e][t] = 0.0;
                    resiStrategy[tide][e][t] = 0.0;
                }
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
            /* double tProp = t/tSteps;
            pFemMax[tide][t] = intercept + (slp * tProp) + (slp2 * pow(tProp, 2)) + (slp3 * pow(tProp, 3));
            if(pFemMax[tide][t] > 1) std::cout << "Problem: pFemMax is bigger than it should be"; */

            pFemMaxList[tide][t] = pFemMax;
            pFemMinList[tide][t] = pFemMin; 

            /* if(t>25)
            {
                pFemMaxList[tide][t] = pFemMax;
                pFemMinList[tide][t] = pFemMin; 
            }else
            {
                pFemMaxList[tide][t] = 0.0;
                pFemMinList[tide][t] = 0.0; 
            } */
        }
    }

    //START OF WHILE LOOP

    while(maxDiffFit > 0.000001 && counter < countMax)
    {
        //Empty out the frequency distribution array at the start of each loop

        for(int tide=0; tide<tides; tide++)
        {
            for(int t=0; t<tSteps; t++)
            {
                pMate[tide][t] = 0.0;
                Rt[tide][t] = 0.0;

                for(int e=0; e<eMax; e++)
                {
                    if(t==(tSteps-1) & e>0 & tide==(tides-1))
                    {
                        timeoutFitness[tide][e][t] = 1.0;
                        waveFitness[tide][e][t] = 1.0;
                        forFitness[tide][e][t] = 1.0;
                        BRfitness[tide][e][t] = 1.0;
                        resiFitness[tide][e][t] = 1.0;
                        resiTimeoutFitness[tide][e][t] = 1.0;
                    }else
                    {
                        timeoutFitness[tide][e][t] = 0.0;
                        waveFitness[tide][e][t] = 0.0;
                        forFitness[tide][e][t] = 0.0;
                        BRfitness[tide][e][t] = 0.0;
                        resiFitness[tide][e][t] = 0.0;
                        resiTimeoutFitness[tide][e][t] = 0.0;
                    }

                    for(int i=0; i<2; i++)
                    {
                        freqDist[i][tide][e][t] = 0.0;
                    }
                }
            }
        }

        //If we're at the very start, set the arbitrary starting strategy
        if(counter==0)
        {
            resiStrategy = StartingStrat(resiStrategy, tides, eMax, tSteps);
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
                                                alpha, 
                                                ptau,
                                                postMatingTimeout,     
                                                binom,
                                                Rt,
                                                pMate,
                                                pFemMaxList, 
                                                pFemMinList,
                                                HTCprob,
                                                resiStrategy, 
                                                freqDist,
                                                pMort, 
                                                pWaveMort);

        //Save the output of the above function
        freqDist = markovOutput->array1;
        pMate = markovOutput->array2;

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
                        //std::cout << "before BRfit final timestep = " << BRfitness[tides-1][e][tSteps-1] << std::endl;
                        //std::cout << "BRfit before = " << BRfitness[tide][e][t] << "\n";
                        BRfitness[tide][e][t] = HighTide(tides, tide, e, BRfitness, HTCprob, HTcost, stochasticHTcost);
                        //std::cout << "BRfit after = " << BRfitness[tide][e][t] << "\n\n";
                        resiFitness[tide][e][t] = HighTide(tides, tide, e, resiFitness, HTCprob, HTcost, stochasticHTcost);

                        if(postMatingTimeout == true)
                        {
                            timeoutFitness[tide][e][t] = HighTide(tides, tide, e, timeoutFitness, HTCprob, HTcost, stochasticHTcost);
                            resiTimeoutFitness[tide][e][t] = HighTide(tides, tide, e, resiTimeoutFitness, HTCprob, HTcost, stochasticHTcost);
                        }
                    }
                    else //if we're not in the final timestep of a tide
                    {
                        if(postMatingTimeout == true)
                        {
                            //std::cout << "postMatingTimeout fitness for other timesteps\n";
                            timeoutFitness[tide][e][t] = TimeoutFitness(tide, t, e, metCostProb, timeoutFitness, BRfitness, ptau, pMort);
                            resiTimeoutFitness[tide][e][t] = TimeoutFitness(tide, t, e, metCostProb, resiTimeoutFitness, resiFitness, ptau, pMort);
                        }
                        
                        waveFitness[tide][e][t] = WaveFitness(tide, t, e, timeoutFitness, BRfitness, waveCost, metCostProb, pMate, mateBonus, pMort, pWaveMort, postMatingTimeout);
                        forFitness[tide][e][t] = ForageFitness(tide, t, e, eMax, BRfitness, n, binom, pMort, metCostProb);

                        resiWaveFitness[tide][e][t] = WaveFitness(tide, t, e, resiTimeoutFitness, resiFitness, waveCost, metCostProb, pMate, mateBonus, pMort, pWaveMort, postMatingTimeout);
                        resiForFitness[tide][e][t] = ForageFitness(tide, t, e, eMax, resiFitness, n, binom, pMort, metCostProb);


                        double fitDiff = forFitness[tide][e][t] - waveFitness[tide][e][t];
                        double fitDiffErr = fitDiff/error;
                        double exponent = exp(fitDiffErr);
                        double exponentPlusOne = exponent + 1;
                        BRstrategy[tide][e][t] = 1/exponentPlusOne;

                        //std::cout << "BRfit before = " << BRfitness[tide][e][t] << "\n";

                        BRfitness[tide][e][t] = (BRstrategy[tide][e][t] * waveFitness[tide][e][t]) + ((1-BRstrategy[tide][e][t]) * forFitness[tide][e][t]);

                        //std::cout << "BRfit after = " << BRfitness[tide][e][t] << "\n\n";

                        resiFitness[tide][e][t] = (resiStrategy[tide][e][t] * resiWaveFitness[tide][e][t]) + ((1-resiStrategy[tide][e][t]) * resiForFitness[tide][e][t]);

                    }
                }
            }
        }
        maxDiffStrat = 0.0;
        maxDiffFit = 0.0;
        for(int tide=0; tide<tides; tide++)
        {
            for(int t=0; t<(tSteps-1); t++)
            {
                for(int e=1; e<eMax; e++)
                {
                    double diffStrat = abs(BRstrategy[tide][e][t] - resiStrategy[tide][e][t]);
                    double diffFit = abs(BRfitness[tide][e][t] - resiFitness[tide][e][t]);
                    //std::cout << "diffFit = " << diffFit << "\n";
                    //std::cout << "resiFit = " << resiFitness[tide][e][t] << "\n" << "BRfit = " << BRfitness[tide][e][t] << "\n\n";

                    if(diffStrat>maxDiffStrat)
                    {
                        maxDiffStrat = diffStrat;
                        
                    }
                    if(diffFit>maxDiffFit)
                    {
                        maxDiffFit = diffFit;
                        biggestCell[0] = e;
                        biggestCell[1] = t;
                        //std::cout << "biggestCell[e,t] = " << biggestCell[0] << " ," << biggestCell[1] << "\n";
                    }
                }
            }
        }
        for(int tide=0; tide<tides; tide++)
        {     
            for(int t=0; t<tSteps; t++)
            {
                for(int e=0; e<eMax; e++)
                {
                    resiStrategy[tide][e][t] = (lam * BRstrategy[tide][e][t]) + ((1-lam) * resiStrategy[tide][e][t]);
                }
            }
        }
        counter++;

        if(counter % 10 == 0){
            std::cout << "counter = " << counter << ", maxDiffFit = " << maxDiffFit << "\n";
            auto timeNow = std::chrono::system_clock::now();
            std::chrono::duration<double> elapsed_seconds = timeNow-start;
            std::time_t end_time = std::chrono::system_clock::to_time_t(timeNow);
            std::cout << "elapsed time = " << (elapsed_seconds.count())/60 <<"mins \n\n";
            //std::cout << "BRstrat[50][1] = " << BRstrategy[50][1] << "\n\n";
        }
    } //end of while loop

    std::cout << " final counter = " << counter << ", final maxDiffFit = " << maxDiffFit << "\n";
    auto timeNow = std::chrono::system_clock::now();
    std::chrono::duration<double> elapsed_seconds = timeNow-start;
    std::time_t end_time = std::chrono::system_clock::to_time_t(timeNow);
    std::cout << " total elapsed time = " << (elapsed_seconds.count())/60 <<"mins \n";
/*     std::cout << "biggestDiff is in cell e = " << biggestCell[0] << ", t = " << biggestCell[1] << "\n"; */

    for(int i=0; i<2; i++)
    {
        for(int tide=0; tide<tides; tide++)
        {
            for(int e=0; e<eMax; e++)
            {
                for(int t=0; t<tSteps; t++)
                {
                    freqDist[i][tide][e][t] = 0.0;
                }
            }
        }
    }
    freqDist[0][0][(eMax-1)/2][0] = 1.0;

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
                                                alpha, 
                                                ptau,
                                                postMatingTimeout,     
                                                binom,
                                                Rt,
                                                pMate,
                                                pFemMaxList, 
                                                pFemMinList,
                                                HTCprob,
                                                resiStrategy, 
                                                freqDist,
                                                pMort,
                                                pWaveMort);

    freqDist = markovOutput2->array1;
    pMate = markovOutput2->array2;
    Rt = markovOutput2->array3;

    std::ofstream BRstratOut;
    std::ostringstream BRstratOutFilename;
    BRstratOutFilename << "output_files/BRstrat.txt"; 
    BRstratOut.open (BRstratOutFilename.str());
    for(int e=0; e<eMax; e++)
    {
        BRstratOut << BRstrategy[0][e][0];
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
                BRstratOut << "," << BRstrategy[tide][e][t];
            }
            BRstratOut << "," << "HT";
        }
        BRstratOut << std::endl;
    }
    BRstratOut.close();

    std::ofstream resiStratOut;
    std::ostringstream resiStratOutFilename;
    resiStratOutFilename << "output_files/resiStrat.txt"; 
    resiStratOut.open (resiStratOutFilename.str());
    for(int e=0; e<eMax; e++)
    {
        resiStratOut << resiStrategy[0][e][0];
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
                resiStratOut << "," << resiStrategy[tide][e][t];
            }
            resiStratOut << "," << "HT";
        }
        resiStratOut << std::endl;
    }
    resiStratOut.close();


    std::ofstream freqDistOut;
    outfile.str("output_files/freqDist.txt");
    std::string freqDistOutFilename = outfile.str();
    freqDistOut.open (freqDistOutFilename.c_str());
        for(int e=0; e<eMax; e++)
        {
            freqDistOut << freqDist[0][0][e][0];
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
                    freqDistOut << "," << freqDist[0][tide][e][t];
                }
                freqDistOut << "," << "HT";
            }
            freqDistOut << std::endl;
        }
    freqDistOut.close(); 

    std::ofstream freqDistTimeout;
    outfile.str("output_files/freqDistTimeout.txt");
    std::string freqDistTimeoutFilename = outfile.str();
    freqDistTimeout.open (freqDistTimeoutFilename.c_str());
        for(int e=0; e<eMax; e++)
        {
            freqDistTimeout << freqDist[1][0][e][0];
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
                    freqDistTimeout << "," << freqDist[1][tide][e][t];
                }
                freqDistTimeout << "," << "HT";
            }
            freqDistTimeout << std::endl;
        }
    freqDistTimeout.close(); 

    std::ofstream waveFit;
    outfile.str("output_files/waveFit.txt");
    std::string waveFitFilename = outfile.str();
    waveFit.open (waveFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        waveFit << waveFitness[0][e][0];
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
                waveFit << "," << waveFitness[tide][e][t];
            }
            waveFit << "," << "HT";
        }
        waveFit << std::endl;
    }
    waveFit.close(); 

    std::ofstream forFit;
    outfile.str("output_files/forFit.txt");
    std::string forFitFilename = outfile.str();
    forFit.open (forFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        forFit << forFitness[0][e][0];
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
                forFit << "," << forFitness[tide][e][t];
            }
            forFit << "," << "HT";
        }
        forFit << std::endl;
    }   
    forFit.close();

    std::ofstream BRFit;
    outfile.str("output_files/BRFit.txt");
    std::string BRFitFilename = outfile.str();
    BRFit.open (BRFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        BRFit << BRfitness[0][e][0];
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
                BRFit << "," << BRfitness[tide][e][t];
            }
            BRFit << "," << "HT";
        }
        BRFit << std::endl;
    }
    BRFit.close(); 

    std::ofstream ResiFit;
    outfile.str("output_files/ResiFit.txt");
    std::string ResiFitFilename = outfile.str();
    ResiFit.open (ResiFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        ResiFit << resiFitness[0][e][0];
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
                ResiFit << "," << resiFitness[tide][e][t];
            }
            ResiFit << "," << "HT";
        }
        ResiFit << std::endl;
    }
    ResiFit.close(); 

    std::ofstream TimeoutFit;
    outfile.str("output_files/timeoutFit.txt");
    std::string TimeoutFitFilename = outfile.str();
    TimeoutFit.open (TimeoutFitFilename.c_str());
    for(int e=0; e<eMax; e++)
    {
        TimeoutFit << timeoutFitness[0][e][0];
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
                TimeoutFit << "," << timeoutFitness[tide][e][t];
            }
            TimeoutFit << "," << "HT";
        }
        TimeoutFit << std::endl;
    }
    TimeoutFit.close(); 

    std::ofstream pMateOut;
    outfile.str("output_files/finalpMate.txt");
    std::string pMateFilename = outfile.str();
    pMateOut.open(pMateFilename.c_str());
    pMateOut << pMate[0][0] << ",";
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
            pMateOut << pMate[tide][t] << ",";
        }
        pMateOut << "," << "HT" << ",";
    }
    pMateOut.close();

    std::ofstream RtOut;
    outfile.str("output_files/finalRt.txt");
    std::string RtFilename = outfile.str();
    RtOut.open(RtFilename.c_str());
    RtOut << Rt[0][0] << ",";
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
            RtOut << Rt[tide][t] << ",";
        }
        RtOut << "," << "HT" << ",";
    }
    RtOut.close();

    double propNotDead;

    for(int tide = 0; tide < tides; tide++)
    {
        for(int t = 0; t< tSteps; t++)
        {
            phiDead[tide][t] = freqDist[0][tide][0][t] + freqDist[1][tide][0][t];

            propNotDead = 0.0;
            for(int e = 1; e < eMax; e++)
            {
                propNotDead += freqDist[0][tide][e][t] + freqDist[1][tide][e][t];
            }

            phiDead[tide][t] += propNotDead * pMort;
        }
    }

    std::ofstream phiDeadOut;
    outfile.str("output_files/phiDead.txt");
    std::string phiDeadFilename = outfile.str();
    phiDeadOut.open(phiDeadFilename.c_str());
    phiDeadOut << Rt[0][0] << ",";
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
            phiDeadOut << phiDead[tide][t] << ",";
        }
        phiDeadOut << "," << "HT" << ",";
    }
    phiDeadOut.close();

    double propMating;
    for(int tide = 0; tide < tides; tide++)
    {
        for(int t = 0; t < tSteps; t++)
        {
            propMating = 0;
            for(int e = 0; e < eMax; e++)
            {
                propMating += freqDist[0][tide][e][t] * BRstrategy[tide][e][t] * pMate[tide][t];
            }
            phiMate[tide][t] = propMating;
        }
    }

    std::ofstream phiMateOut;
    outfile.str("output_files/phiMate.txt");
    std::string phiMateFilename = outfile.str();
    phiMateOut.open(phiMateFilename.c_str());
    phiMateOut << Rt[0][0] << ",";
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
            phiMateOut << phiMate[tide][t] << ",";
        }
        phiMateOut << "," << "HT" << ",";
    }
    phiMateOut.close();

}