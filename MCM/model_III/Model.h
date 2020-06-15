//
//  ModelA.h
//  Math42_Model3
//
//  Created by Christine Mika Hamakawa on 6/12/20.
//  Copyright 2020 Christine Mika Hamakawa. All rights reserved.
//

#ifndef Model_h
#define Model_h
#include <iostream>
#include <vector>
using namespace std;

class Model {
public:
    
    //modelA constructor
    Model(double c0, vector<double> herring, 
        vector <double> mackerel, double input_k): h(herring),m(mackerel) {
        c_0 = c0;
        k = input_k;
        //no relocation year because no relocation
    }
    
    //modelB contructor
    Model(double c0,double input_a,
        vector<double> herring, vector <double> mackerel, vector<double> h0, 
        vector<double>m0, double input_k): h(herring),m(mackerel),h_0(h0),m_0(m0){
        c_0 = c0;
        a = input_a;
        k = input_k;
        X = 3; //relocation year is 2023
    }
    
    
    //writes year and profit to file
    void data(string filename) {
        ofstream output;
        output.open(filename);
        for(int i = 0; i<years; i++) {
            double profit = 0;
            if(i<=X) {
                profit = totalRevenue(h[i], m[i]) - totalCost();
                cout<<h[i]<<endl;
            }
            else {
                profit = totalRevenue(h_0[i], m_0[i])-totalCost();
            }
            output<<init_year+i<<","<<profit<<endl;
        }
        output.close();
        
    }
    
    //calculates annual total cost
    double totalCost() {
    double cost1 = 0;
        if(X==51) {
            cost1 = 0;
        }
        else {
            cost1 = a*1.0/51;
        }
        double cost2 = c_0*1.0/15;
        
        return (cost1+cost2);

    }
    
    //calculates annual revenue
    double totalRevenue(double q_h, double q_m) {
        return (k*((h_price*q_h) + (m_price*q_m)));
    }
    
    
private:
    //fish prices
    double h_price = 1;
    double m_price = 1;
    
    int k = 0;  				//number of times fish per year
    const int years = 51;  		//number of years (size of vector, includes 0)
    const int init_year = 2019; //initial year (year 0)
    double X = 51; 				//relocation year (in respects to 2019)

    //fixed costs
    double c_0 = 0; //boat
    double a = 0; 	//relocation
    
    //q vectors for herring and mackerel
    vector<double> h;
    vector<double> m;
    
    //relocation:  q vectors for herring and mackerel after relocation
    vector<double> h_0;
    vector<double> m_0;
    
    
};

#endif /* ModelA_h */
