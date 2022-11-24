#include <iostream>   

int Fact(int x)
{  
    int fact = 1;
    for(int i=1;i<=x;i++)
    {    
        fact *= i;    
    }    
  return fact;  
}  