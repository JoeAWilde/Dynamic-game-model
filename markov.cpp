#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <math.h> 
#include <bits/stdc++.h>
#include <chrono>
#include <ctime> 
#include "functions.h"
#include "arrayContainer.h"

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
                double pWaveMort)
                
{
    int tideIndex;
    int timeIndex;

    freqDist[0][0][(eMax-1)/2][0] = 1.0; //put the whole populaton at the middle energy level

    for(int tide=0; tide<tides; tide++)
    {
        int tStop;
        if(tide == tides-1) 
        {
            tStop = tSteps-2;
        }
        else
        {
            tStop = tSteps-1;
        }
        for(int t=0; t<=tStop; t++) //cycle forward through the timesteps
        {
            tideIndex = tide;
            timeIndex = t+1;

            if(timeIndex >= tSteps) //if we cross a tide boundary
            {
                tideIndex = tide + 1;
                timeIndex = 0;
                
                if(tideIndex >= tides) //this should never happen because the timeIndex should never go out of tide bounds on the final low tide
                {
                    tideIndex = tides-1;
                    timeIndex = tSteps-1;
                }

                double propSum = 0.0;

                //if in the final timestep of a tide, do high tide cost to all non-dead population
                for(int e=1; e<eMax; e++) 
                {
                    if(stochasticHTcost==true)
                    {
                        for(int x=0; x<3; x++) //cycle through possible high tide costs
                        {
                            int eHTindex = e - (HTcost-2+x);


                            if(eHTindex<0) eHTindex=0;

                            for(int i=0; i<2; i++) //apply high tide cost to both non and timeout arrays
                            {
                                //std::cout << "i = " << i << ", tideIndex = " << tideIndex << ", eHTindex = " << eHTindex << ", timeIndex = " << timeIndex << std::endl;
                                freqDist[i][tideIndex][eHTindex][timeIndex] += (freqDist[i][tide][e][t] * HTCprob[x]);
                            }
                        }
                    }
                    else
                    {
                        int eHTindex = e - HTcost;

                        if(eHTindex<0) eHTindex=0;

                        for(int i=0; i<2; i++)
                        {
                            freqDist[i][tideIndex][eHTindex][timeIndex] += freqDist[i][tide][e][t];
                        }
                    }
                }
            }
            else //if we don't go across a tide boundary
            {
                double RtSum = 0.0; //set the proportion of rivals waving in each timestep to zero
                for(int e=1; e<eMax; e++)
                {
                    RtSum += (resiStrategy[tide][e][t]*freqDist[0][tide][e][t]); //add up the products of the probability of waving and the frequency of the 
                                                                    //population that have that probability of waving
                }
                Rt[tide][t] = RtSum; //set the total proportion of rivals waving for that timestep
                double powerTop = pow(Rt[tide][t],alpha); //calculate the top power part of the pMate function
                double pMateTop = (pFemMinList[tide][t] + powerTop*(pFemMaxList[tide][t] - pFemMinList[tide][t])); //calculate the numerator for pMate function
                double pMateBott = Rt[tide][t] + b; //calculate the denominator for pMate function

                pMate[tide][t] = pMateTop / pMateBott; //calculate pMate from Rt

                if(pMate[tide][t] > 1.0) pMate[tide][t] = 1.0; //resets probability of mating within 1

                for(int e=0; e<eMax; e++) //cycle through all energy levels
                {          
                    //FORAGING
                    for(int k=0; k<=n; k++)
                    {
                        for(int m=0; m<3; m++)
                        {
                            int eForIndex = e + k - m;

                            if(eForIndex>=eMax) eForIndex = eMax-1;
                            if(eForIndex<0) eForIndex = 0;
                        
                            freqDist[0][tideIndex][eForIndex][timeIndex] += (freqDist[0][tide][e][t] * (1-resiStrategy[tide][e][t]) * binom[k] * (1-pMort) * metCostProb[m]);
                        }
                    }
                    //WAVING
                    for(int m=0; m<3; m++)
                    {
                        int eWaveIndex = e - waveCost - m; //energy level at t+1 

                        if(eWaveIndex<0) eWaveIndex=0; //reset within bounds

                        //those who don't mate
                        freqDist[0][tideIndex][eWaveIndex][timeIndex] += (freqDist[0][tide][e][t] * resiStrategy[tide][e][t] * (1-pMate[tide][t]) * (1-(pMort*pWaveMort)) * metCostProb[m]);
                        
                        if(postMatingTimeout == true)
                        {
                            //those who do mate
                            freqDist[1][tideIndex][eWaveIndex][timeIndex] += (freqDist[0][tide][e][t] * resiStrategy[tide][e][t] * pMate[tide][t] * (1-(pMort*pWaveMort)) * metCostProb[m]);
                        }else
                        {
                            //those who do mate
                            freqDist[0][tideIndex][eWaveIndex][timeIndex] += (freqDist[0][tide][e][t] * resiStrategy[tide][e][t] * pMate[tide][t] * (1-(pMort*pWaveMort)) * metCostProb[m]);
                        }
                    }
                        
                    //MOVING TIMEOUT PROPORTION
                    if(postMatingTimeout == true)
                    {
                        for(int m=0; m<3; m++)
                        {
                            int eTimeout = e - m; //lose on energy unit staying in timeout

                            if(eTimeout<0) eTimeout=0; //reset within bounds

                            //those who leave timeout
                            freqDist[0][tideIndex][eTimeout][timeIndex] += (freqDist[1][tide][e][t] * ptau * (1-pMort) * metCostProb[m]);

                            //those who stay in timeout
                            freqDist[1][tideIndex][eTimeout][timeIndex] += (freqDist[1][tide][e][t] * (1-ptau) * (1-pMort) * metCostProb[m]);
                        }
                    }
                } 
            }
            //Move proportion that die due to background mortality
            double propForSum = 0.0;
            double propWavSum = 0.0;
            double propTimeoutSum = 0.0;
            for(int e=1; e<eMax; e++)
            {
                propForSum += (freqDist[0][tide][e][t] * (1-resiStrategy[tide][e][t])); //add up proportion of non-dead that are foraging
                propWavSum += (freqDist[0][tide][e][t] * resiStrategy[tide][e][t]);

                if(postMatingTimeout == true)
                {
                    propTimeoutSum += freqDist[1][tide][e][t];
                }
            }
            freqDist[0][tideIndex][0][timeIndex] += (propForSum * pMort) + (propWavSum * (pMort * pWaveMort)) + (propTimeoutSum * pMort); //put the dead in e=0 in the next time step

            double propAliveNonTimeout = 0.0;
            double propAliveTimeout = 0.0;
            for(int e = 0; e < eMax; e++)
            {
                propAliveNonTimeout += freqDist[0][tide][e][t];
                if(postMatingTimeout == true)
                {
                    propAliveTimeout += freqDist[1][tide][e][t];
                }
            }

            for(int e = 0; e<eMax; e++) //rescale the next timestep so that it is a proportion of those alive
            {
                freqDist[0][tideIndex][e][t] = (freqDist[0][tideIndex][e][t]/propAliveNonTimeout);

                if(postMatingTimeout == true)
                {
                    freqDist[1][tideIndex][e][t] = (freqDist[1][tideIndex][e][t]/propAliveTimeout);
                }
            }

        }
    }
    ArrayContainer* masterArray = new ArrayContainer();

    masterArray->array1 = freqDist;
    masterArray->array2 = pMate;
    masterArray->array3 = Rt;

    return masterArray;
}
    