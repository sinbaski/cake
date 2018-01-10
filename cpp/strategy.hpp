#ifndef strategy_hpp
#define strategy_hpp

#include <exception>
#include <string>
#include <vector>
#include <SQLAPI.h> // main SQLAPI++ header

#include "DataSource.hpp"

using namespace std;

typedef pair<string, int> action_point;

class action_list
{
public:
    vector<action_point> list;
};

template<class T=double>
class mutation_gen
{
public:
    virtual T mutate(T old) = 0;
};

class normal_mutation_gen : public mutation_gen<double>
{
public:
    double mu;
    double sd;
    double mutate(double old);
};

class lognormal_mutation_gen : public mutation_gen<double>
{
public:
    double mu;
    double sd;
    double mutate(double old);
};

class variable_param
{
public:
    virtual void mutate(void) = 0;
    virtual operator=(const variable_param &) = 0;
};

class real_param : public variable_param
{
public:
    double value;
    mutation_gen<double> *mutor;
    real_param(double value);
    real_param(double value, const *mutor);
};

class strategy
{
public:
    map<string, variable_param> parameters;
    // Called when new data come in
    virtual void update(void) = 0;
    virtual action_list act(void) = 0;
    const string race;
};

class mikosch_strat : public strategy
{
protected:
    DataSource ds;
public:
    mikosch_strat(void);
    mikosch_strat(const map<string, variable_param>& parameters);
};

#endif
