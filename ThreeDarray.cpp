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

ArrayContainer* ThreeDarray(int tides, int tSteps, int eMax)
{
    double ****fourDholder = new double ***[2];
    for(int i=0; i<2; i++)
    {
        fourDholder[i] = new double **[tides];
        for(int x=0; x<tides; x++)
        {
            fourDholder[i][x] = new double *[eMax];   //create a pointer that is eMax units long
            for(int e=0; e<eMax; e++) //in each of those indices
            {
                fourDholder[i][x][e] = new double[tSteps]; //make a pointer that is tSteps units long
            }
        }
    }


    double ***threeDholder = new double **[tides];
    for(int x=0; x<tides; x++)
    {
        threeDholder[x] = new double *[eMax];   //create a pointer that is eMax units long
        for(int i=0; i<eMax; i++) //in each of those indices
        {
            threeDholder[x][i] = new double[tSteps]; //make a pointer that is tSteps units long
        }
    }

    double **twoDholder = new double *[tides];
    for(int x=0; x<tides; x++)
    {
        twoDholder[x] = new double [tSteps];
    }


    ArrayContainer* masterArray = new ArrayContainer();
    masterArray -> array1 = fourDholder;
    masterArray -> array2 = twoDholder;
    masterArray -> array4 = threeDholder;
    return masterArray;
}