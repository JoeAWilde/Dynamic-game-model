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
                double theta, 
                double ptau,
                bool postMatingTimeout,     
                double* binom,
                double** phiLargeWav,
                double** phiSmallWav,
                double** largePMate,
                double** smallPMate,
                double** pFemMaxList, 
                double** pFemMinList,
                double* HTCprob,
                double*** largeResiStrategy, 
                double*** smallResiStrategy,
                double**** largeFreqDist,
                double**** smallFreqDist,
                double q,
                double alpha,
                double pMort, 
                double pWaveMort)
                
{
    int tideIndex;
    int timeIndex;

    largeFreqDist[0][0][(eMax-1)/2][0] = q; //put the whole populaton at the middle energy level
    smallFreqDist[0][0][(eMax-1)/2][0] = (1-q);

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
                                largeFreqDist[i][tideIndex][eHTindex][timeIndex] += (largeFreqDist[i][tide][e][t] * HTCprob[x]);
                                smallFreqDist[i][tideIndex][eHTindex][timeIndex] += (smallFreqDist[i][tide][e][t] * HTCprob[x]);
                            }
                        }
                    }
                    else //if using fixed high tide cost
                    {
                        int eHTindex = e - HTcost;

                        if(eHTindex<0) eHTindex=0;

                        for(int i=0; i<2; i++)
                        {
                            largeFreqDist[i][tideIndex][eHTindex][timeIndex] += largeFreqDist[i][tide][e][t];
                            smallFreqDist[i][tideIndex][eHTindex][timeIndex] += smallFreqDist[i][tide][e][t];
                        }
                    }
                }
            }
            else //if we don't go across a tide boundary
            {
                double propLargeWaving = 0.0; //set the proportion of rivals waving in each timestep to zero
                double propSmallWaving = 0.0;
                for(int e=1; e<eMax; e++)
                {
                    propLargeWaving += (largeResiStrategy[tide][e][t]*(largeFreqDist[0][tide][e][t]/q)); //add up the products of the probability of waving and the frequency of the 
                                                                    //population that have that probability of waving
                    propSmallWaving += (smallResiStrategy[tide][e][t]*(smallFreqDist[0][tide][e][t]/(1-q)));
                }
                phiLargeWav[tide][t] = propLargeWaving; //set the total proportion of rivals waving for that timestep
                phiSmallWav[tide][t] = propSmallWaving; //set the total proportion of rivals waving for that timestep

                double effectOfWavers = (phiLargeWav[tide][t] * q) + ((1-alpha) * phiSmallWav[tide][t] * (1-q));
                double pMateTopLine = pFemMinList[tide][t] + (pow(effectOfWavers, theta) * (pFemMaxList[tide][t] - pFemMinList[tide][t]));
                double pMateBottomLine = effectOfWavers + b;



                largePMate[tide][t] = pMateTopLine / pMateBottomLine; //calculate pMate from Rt
                smallPMate[tide][t] = (pMateTopLine / pMateBottomLine) * (1 - alpha);

                if(largePMate[tide][t] > 1.0) largePMate[tide][t] = 1.0; //resets probability of mating within 1
                if(smallPMate[tide][t] > 1.0) smallPMate[tide][t] = 1.0; //resets probability of mating within 1

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
                        
                            largeFreqDist[0][tideIndex][eForIndex][timeIndex] += (largeFreqDist[0][tide][e][t] * (1-largeResiStrategy[tide][e][t]) * binom[k] * (1-pMort) * metCostProb[m]);
                            smallFreqDist[0][tideIndex][eForIndex][timeIndex] += (smallFreqDist[0][tide][e][t] * (1-smallResiStrategy[tide][e][t]) * binom[k] * (1-pMort) * metCostProb[m]);

                        }
                    }
                    //WAVING
                    for(int m=0; m<3; m++)
                    {
                        int eWaveIndex = e - waveCost - m; //energy level at t+1 

                        if(eWaveIndex<0) eWaveIndex=0; //reset within bounds

                        //those who don't mate
                        largeFreqDist[0][tideIndex][eWaveIndex][timeIndex] += (largeFreqDist[0][tide][e][t] * largeResiStrategy[tide][e][t] * (1-largePMate[tide][t]) * (1-(pMort*pWaveMort)) * metCostProb[m]);
                        smallFreqDist[0][tideIndex][eWaveIndex][timeIndex] += (smallFreqDist[0][tide][e][t] * smallResiStrategy[tide][e][t] * (1-smallPMate[tide][t]) * (1-(pMort*pWaveMort)) * metCostProb[m]);

                        if(postMatingTimeout == true)
                        {
                            //those who do mate
                            largeFreqDist[1][tideIndex][eWaveIndex][timeIndex] += (largeFreqDist[0][tide][e][t] * largeResiStrategy[tide][e][t] * largePMate[tide][t] * (1-(pMort*pWaveMort)) * metCostProb[m]);
                            smallFreqDist[1][tideIndex][eWaveIndex][timeIndex] += (smallFreqDist[0][tide][e][t] * smallResiStrategy[tide][e][t] * smallPMate[tide][t] * (1-(pMort*pWaveMort)) * metCostProb[m]);
                        }else
                        {
                            //those who do mate
                            largeFreqDist[0][tideIndex][eWaveIndex][timeIndex] += (largeFreqDist[0][tide][e][t] * largeResiStrategy[tide][e][t] * largePMate[tide][t] * (1-(pMort*pWaveMort)) * metCostProb[m]);
                            smallFreqDist[0][tideIndex][eWaveIndex][timeIndex] += (smallFreqDist[0][tide][e][t] * smallResiStrategy[tide][e][t] * smallPMate[tide][t] * (1-(pMort*pWaveMort)) * metCostProb[m]);
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
                            largeFreqDist[0][tideIndex][eTimeout][timeIndex] += (largeFreqDist[1][tide][e][t] * ptau * (1-pMort) * metCostProb[m]);
                            smallFreqDist[0][tideIndex][eTimeout][timeIndex] += (smallFreqDist[1][tide][e][t] * ptau * (1-pMort) * metCostProb[m]);

                            //those who stay in timeout
                            largeFreqDist[1][tideIndex][eTimeout][timeIndex] += (largeFreqDist[1][tide][e][t] * (1-ptau) * (1-pMort) * metCostProb[m]);
                            smallFreqDist[1][tideIndex][eTimeout][timeIndex] += (smallFreqDist[1][tide][e][t] * (1-ptau) * (1-pMort) * metCostProb[m]);

                        }
                    }
                } 
            }
            //Move proportion that die due to background mortality
            double largePropForSum = 0.0;
            double largePropWavSum = 0.0;
            double largePropTimeoutSum = 0.0;

            double smallPropForSum = 0.0;
            double smallPropWavSum = 0.0;
            double smallPropTimeoutSum = 0.0;
            for(int e=1; e<eMax; e++)
            {
                largePropForSum += (largeFreqDist[0][tide][e][t] * (1-largeResiStrategy[tide][e][t])); //add up proportion of non-dead that are foraging
                smallPropForSum += (smallFreqDist[0][tide][e][t] * (1-smallResiStrategy[tide][e][t])); //add up proportion of non-dead that are foraging

                largePropWavSum += (largeFreqDist[0][tide][e][t] * largeResiStrategy[tide][e][t]);
                smallPropWavSum += (smallFreqDist[0][tide][e][t] * smallResiStrategy[tide][e][t]);


                if(postMatingTimeout == true)
                {
                    largePropTimeoutSum += largeFreqDist[1][tide][e][t];
                    smallPropTimeoutSum += smallFreqDist[1][tide][e][t];
                }
            }
            largeFreqDist[0][tideIndex][0][timeIndex] += (largePropForSum * pMort) + (largePropWavSum * (pMort * pWaveMort)) + (largePropTimeoutSum * pMort); //put the dead in e=0 in the next time step
            smallFreqDist[0][tideIndex][0][timeIndex] += (smallPropForSum * pMort) + (smallPropWavSum * (pMort * pWaveMort)) + (smallPropTimeoutSum * pMort); //put the dead in e=0 in the next time step


            double largePropAliveNonTimeout = 0.0;
            double smallPropAliveNonTimeout = 0.0;

            double largePropAliveTimeout = 0.0;
            double smallPropAliveTimeout = 0.0;

            for(int e = 0; e < eMax; e++)
            {
                largePropAliveNonTimeout += largeFreqDist[0][tide][e][t];
                smallPropAliveNonTimeout += smallFreqDist[0][tide][e][t];

                if(postMatingTimeout == true)
                {
                    largePropAliveTimeout += largeFreqDist[1][tide][e][t];
                    smallPropAliveTimeout += smallFreqDist[1][tide][e][t];

                }
            }

            for(int e = 0; e<eMax; e++) //rescale the next timestep so that it is a proportion of those alive
            {
                largeFreqDist[0][tideIndex][e][t] = (largeFreqDist[0][tideIndex][e][t]/largePropAliveNonTimeout);
                smallFreqDist[0][tideIndex][e][t] = (smallFreqDist[0][tideIndex][e][t]/smallPropAliveNonTimeout);

                if(postMatingTimeout == true)
                {
                    largeFreqDist[1][tideIndex][e][t] = (largeFreqDist[1][tideIndex][e][t]/largePropAliveTimeout);
                    smallFreqDist[1][tideIndex][e][t] = (smallFreqDist[1][tideIndex][e][t]/smallPropAliveTimeout);
                }
            }

        }
    }
    ArrayContainer* masterArray = new ArrayContainer();

    masterArray->array1 = largeFreqDist;
    masterArray->array2 = largePMate;
    masterArray->array3 = phiLargeWav;

    masterArray->array4 = smallFreqDist;
    masterArray->array5 = smallPMate;
    masterArray->array6 = phiSmallWav;

    return masterArray;
}
    