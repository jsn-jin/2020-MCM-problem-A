#include <iostream>
#include <fstream>
#include "Model.h"
#include <sstream>
#include <vector>
using namespace std;


//function updates vectors
void createVectors(vector<double>& a,vector<double>&b, 
    vector<double>&c, vector<double>&d, string filename);

int main(){
    
    //create herring vectors
    vector<double> h_current;
    vector<double> h_newBoat;
    vector<double> h_moveNorth;
    vector<double> h_moveSouth;
    createVectors(h_current, h_newBoat, h_moveNorth, 
        h_moveSouth, "q_herring.csv");
    
    //create mackerel vectors
    vector<double> m_current;
    vector<double> m_newBoat;
    vector<double> m_moveNorth;
    vector<double> m_moveSouth;
    createVectors(m_current, m_newBoat, m_moveNorth, 
        m_moveSouth, "q_mackerel.csv");
    
    //k value:  86
    //boats fish 5 days out of seven each week on average
    //boats can go up to three days w/o refrigeration
    //k = 5*52/3
    double old_k = 86;
    
    //k value:  43
    //boats fish 5 days out of seven each week on average
    //boats can go up to six days w/o refrigeration
    //k = 5*52/6
    double new_k = 43;
    
    //fixed costs
    double boat = 50, boat_fridge = 52, port = 14;
    
    /*Constructor for Model A:
      Model(double c0, vector<double> herring, 
      vector <double> mackerel, double input_k);
     */
   //current model
    Model current(boat,h_current,m_current, old_k);
    current.data("current.csv");
    
    //fridge model
    Model newBoat(boat_fridge, h_newBoat,m_newBoat, new_k);
    newBoat.data("newBoat.csv");
    
    
   /*Constructor for Model B:
    Model(double c0,double input_a,vector<double> herring, 
    vector <double> mackerel, vector<double> h0, vector<double>m0, double input_k)
    */
    
    //relocation model
    Model moveNorth(boat,port,h_current,m_current,h_moveNorth, m_moveNorth, old_k);
    moveNorth.data("moveNorth.csv");
    
    return 0;
}

//function updates vectors
void createVectors(vector<double>& a,vector<double>&b, 
    vector<double>&c, vector<double>&d, string filename) {
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
           
