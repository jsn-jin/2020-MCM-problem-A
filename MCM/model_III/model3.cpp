#include <iostream>
#include <fstream>
#include "Model.h"
#include <sstream>
#include <vector>
using namespace std;


// function updates vectors
void createVectors(vector<double>& a,vector<double>&b, 
    vector<double>&c, vector<double>&d, string filename);

int main(){
    
    // create herring vectors
    vector<double> h_current;
    vector<double> h_newBoat;
    vector<double> h_moveNorth;
    vector<double> h_moveSouth;
    createVectors(h_current, h_newBoat, h_moveNorth, 
        h_moveSouth, "q_herring_int_water.csv");
    
    // create mackerel vectors
    vector<double> m_current;
    vector<double> m_newBoat;
    vector<double> m_moveNorth;
    vector<double> m_moveSouth;
    createVectors(m_current, m_newBoat, m_moveNorth, 
        m_moveSouth, "q_mackerel_int_water.csv");
    
    // k value:  86
    // boats fish 5 days out of seven each week on average
    // boats can go up to three days w/o refrigeration
    // k = 5*52/3
    double old_k = 1;
    
    // k value:  43
    // boats fish 5 days out of seven each week on average
    // boats can go up to six days w/o refrigeration
    // k = 5*52/6
    double new_k = .5;
    
    //fixed costs
    double boat = .93, boat_fridge = .96, port = .07;


    // current model
    Model current(boat,h_current,m_current, old_k);
    current.data("current.csv");
    
    // fridge model
    Model newBoat(boat_fridge, h_newBoat,m_newBoat, new_k); 
        newBoat.data("newBoat_int_water.csv");
    
    
    // relocation model
    double relocation_year1 = 2;
    Model moveNorth1(boat,port,h_current,m_current,h_moveNorth, m_moveNorth, 
        old_k, relocation_year1); moveNorth1.data("moveNorth2022.csv");
    // before the density becomes too small
    // bar with highest density 2020
    
    
    // relocation model
    double relocation_year2 = 7;
    Model moveNorth2(boat,port,h_current,m_current,h_moveNorth, m_moveNorth, 
        old_k, relocation_year2); moveNorth2.data("moveNorth2027.csv");
    // before the density becomes too small
    // bar with highest density 2020
    
    
    // relocation model
    double relocation_year3 = 14;
    Model moveNorth3(boat,port,h_current,m_current,h_moveNorth, m_moveNorth, 
        old_k, relocation_year3); moveNorth3.data("moveNorth2034.csv");
    // before the density becomes too small
    // bar with highest density 2020
    
    return 0;
}

// function updates vectors
void createVectors(vector<double>& a,vector<double>&b, vector<double>&c, 
                        vector<double>&d, string filename) {
    ifstream input;
    input.open(filename);
    int count = 0;
    
    while(!input.eof()) {
        string s;
        getline(input, s);
        if(count==0) {
            count++;
            continue;
        }
        else if(count==52) {
            continue;
        }
        else {
            istringstream instr(s);
            double d1, d2, d3, d4, d5;
            char c1, c2, c3, c4, c5,c6;
            instr>>c1>>d1>>c2>>c6>>d2>>c3>>d3>>c4>>d4>>c5>>d5;
            //cout<<d1<<endl;
            a.push_back(d2*1.0/100);
            b.push_back(d3*1.0/100);
            c.push_back(d4*1.0/100);
            d.push_back(d5*1.0/100);
            count++;
        }
    }
    input.close();
}        
