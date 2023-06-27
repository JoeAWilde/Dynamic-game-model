#include <iostream>
#include <random>
#include "functions.h"
#include "arrayContainer.h"

ArrayContainer* Sim(
    int N,
    int eMax, 
    int tSteps, 
    int simTides, 
    int HTcost, 
    double* pMateL,
    double* pMateS, 
    int waveCost, 
    int n, 
    double p,
    bool stochasticHTcost, 
    double b, 
    double theta, 
    double ptau,
    bool postMatingTimeout,     
    double** largeSimStrat, 
    double** smallSimStrat,
    double q,
    double alpha,
    double zeta,
    double pMort, 
    double pWaveMort,
    int*** simEnergy,
    double*** simMating,
    double*** simAlive,
    double*** simTimeout,
    char*** simBehav,
    int* simSizes
    )
{

    // Create a random device
    std::random_device rd;
    // Initialize a random number generator
    std::default_random_engine generator(rd());
    // Create a uniform_real_distribution between 0 and 1
    std::uniform_real_distribution<double> dist(0.0, 1.0);
    // Define a binomial distribution with n and p
    std::binomial_distribution<> binomial_dist(n, p);

    double random_double;

    int tideIndex;
    int tIndex;
    int eHTindex;
    int eIndex;

    for(int z = 0; z < N; z ++)
    {
        simEnergy[0][z][0] = eMax/2;

        for(int tide = 0; tide < simTides; tide ++) 
        {
            int tStop;
            if(tide == simTides-1) //don't do the last timestep in the last tide
            {
                tStop = tSteps-2;
            }
            else
            {
                tStop = tSteps-1;
            }
            for(int t = 0; t <= tStop; t ++)
            {
                //std::cout << "current simEnergy[" << tide << "][" << z << "][" << t << "] = " << simEnergy[tide][z][t] << "\n";
                tideIndex = tide;
                tIndex = t+1;

                if(tIndex >= tSteps) //if we cross a tide boundary
                {
                    tideIndex = tide + 1;
                    tIndex = 0;

                    simMating[tideIndex][z][tIndex] = simMating[tide][z][t];
                    simTimeout[tideIndex][z][tIndex] = simTimeout[tide][z][t];
                }

                //std::cout << "tideIndex = " << tideIndex << ", tIndex = " << tIndex << "\n";
                //std::cout << "BEFORE target simEnergy[" << tideIndex << "][" << z << "][" << tIndex << "] = " << simEnergy[tideIndex][z][tIndex] << "\n";

                

                if(simEnergy[tide][z][t] == 0 ) 
                {
                    //std::cout << "DEAD \n";
                    simAlive[tide][z][t] = 0;
                    simAlive[tideIndex][z][tIndex] = 0;
                    simBehav[tide][z][t] = 'D';
                    simEnergy[tideIndex][z][tIndex] = 0;
                    simMating[tideIndex][z][tIndex] = simMating[tide][z][t];
                    simTimeout[tideIndex][z][tIndex] = 0;
                    //std::cout << "eIndex after dead = " << simEnergy[tideIndex][z][tIndex]<< "\n";
                } 
                else
                {  
                    if(tIndex == 0) //if we cross a tide boundary
                    {
                        //std::cout << "crossed a tide boundary \n";
                        if(stochasticHTcost==true)
                        {
                            random_double = dist(generator);
                            if(random_double < 0.25){
                                eHTindex = simEnergy[tide][z][t] - (HTcost - 1);
                            }
                            else if(random_double > 0.25 && random_double < 0.75)
                            {
                                eHTindex = simEnergy[tide][z][t] - (HTcost);
                            }
                            else if(random_double > 0.75)
                            {
                                eHTindex = simEnergy[tide][z][t] - (HTcost + 1);
                            }
                        }
                        else
                        {
                            eHTindex = simEnergy[tide][z][t] - HTcost;
                        }

                        if(eHTindex<0) eHTindex=0;

                        simEnergy[tideIndex][z][tIndex] = eHTindex; 
                    }
                    else 
                    {
                        if(simTimeout[tide][z][t] == 1)
                        {
                            //std::cout << "TIMEOUT \n";
                            simEnergy[tideIndex][z][tIndex] = simEnergy[tide][z][t];
                            simBehav[tide][z][t] = 'T';
                            simMating[tideIndex][z][tIndex] = simMating[tide][z][t];

                            random_double = dist(generator);
                            if(random_double < ptau)
                            {
                                simTimeout[tideIndex][z][tIndex] = 0;
                            }
                            else
                            {
                                simTimeout[tideIndex][z][tIndex] = 1;
                            }         
                        }
                        else
                        {
                            random_double = dist(generator);
                            if(simSizes[z] == 1)
                            {
                                //std::cout << "energy index for strat = " << simEnergy[tide][z][t] << "\n";
                                if(random_double < largeSimStrat[simEnergy[tide][z][t]][t])
                                {
                                    simBehav[tide][z][t] = 'W';
                                }
                                else
                                {
                                    simBehav[tide][z][t] = 'F';
                                }

                            }
                            else
                            {
                                //std::cout << "energy index for strat = " << simEnergy[tide][z][t] << "\n";
                                if(random_double < smallSimStrat[simEnergy[tide][z][t]][t])
                                {
                                    simBehav[tide][z][t] = 'W';
                                }
                                else
                                {
                                    simBehav[tide][z][t] = 'F';
                                }
                            }

                            if(simBehav[tide][z][t] == 'F')
                            {
                                //std::cout << "FORAGING \n";
                                int forGains = binomial_dist(generator);
                                random_double = dist(generator);

                                if(random_double > 0.25 && random_double < 0.75) forGains = forGains - 1;
                                if(random_double > 0.75) forGains = forGains - 2;
                                //std::cout << "foraging gains = " << forGains << "\n";
                                simEnergy[tideIndex][z][tIndex] = simEnergy[tide][z][t] + forGains;
                                if(simEnergy[tideIndex][z][tIndex] < 0) simEnergy[tideIndex][z][tIndex] = 0;
                                if(simEnergy[tideIndex][z][tIndex] > (eMax-1)) simEnergy[tideIndex][z][tIndex] = eMax-1;

                                simMating[tideIndex][z][tIndex] = simMating[tide][z][t];
                                simTimeout[tideIndex][z][tIndex] = 0;
                            }
                            else
                            {
                                //std::cout << "WAVING \n";
                                int wavCo = waveCost;

                                random_double = dist(generator);
                                if(random_double > 0.25 && random_double < 0.75) wavCo = wavCo + 1;
                                if(random_double > 0.75) wavCo = wavCo + 2;
                                //std::cout << "wave costs = " << wavCo << "\n";

                                simEnergy[tideIndex][z][tIndex] = simEnergy[tide][z][t] - wavCo;
                                if(simEnergy[tideIndex][z][tIndex] < 0) simEnergy[tideIndex][z][tIndex] = 0;
                                if(simEnergy[tideIndex][z][tIndex] > (eMax-1)) simEnergy[tideIndex][z][tIndex] = eMax-1;
                                random_double = dist(generator);
                                if(simSizes[z] == 1){
                                    if(random_double < pMateL[t])
                                    {
                                        simMating[tideIndex][z][tIndex]  = simMating[tide][z][t] + 1;
                                        if(postMatingTimeout)
                                        {
                                            simTimeout[tideIndex][z][tIndex] = 1;
                                        }
                                        else 
                                        {
                                            simTimeout[tideIndex][z][tIndex] = 0;
                                        }
                                    }
                                    else
                                    {
                                        simMating[tideIndex][z][tIndex]  = simMating[tide][z][t];
                                        simTimeout[tideIndex][z][tIndex] = 0;
                                    }

                                }
                                else
                                {
                                    if(random_double < pMateS[t])
                                    {
                                        simMating[tideIndex][z][tIndex]  = simMating[tide][z][t] + 1;
                                        if(postMatingTimeout)
                                        {
                                            simTimeout[tideIndex][z][tIndex] = 1;
                                        }
                                        else 
                                        {
                                            simTimeout[tideIndex][z][tIndex] = 0;
                                        }
                                    }
                                    else
                                    {
                                        simMating[tideIndex][z][tIndex]  = simMating[tide][z][t];
                                        simTimeout[tideIndex][z][tIndex] = 0;
                                    }
                                }
                            }
                        }
                    }
                }
                //std::cout << "AFTER target simEnergy[" << tideIndex << "][" << z << "][" << tIndex << "] = " << simEnergy[tideIndex][z][tIndex] << "\n\n";
            }
            if(simEnergy[tideIndex][z][tIndex] == 0) 
            {
                simAlive[tideIndex][z][tIndex] = 0;
                simEnergy[tideIndex][z][tIndex] = 0;
            }
                //std::cout << "AFTER target simEnergy[" << tideIndex << "][" << z << "][" << tIndex << "] = " << simEnergy[tideIndex][z][tIndex] << "\n\n";
        }
        
    }
    ArrayContainer* masterArray = new ArrayContainer();

    masterArray->array7 = simMating;
    masterArray->array8 = simTimeout;
    masterArray->array9 = simAlive;
    masterArray->array10 = simEnergy;
    masterArray->array11 = simBehav;

    return(masterArray);
}
