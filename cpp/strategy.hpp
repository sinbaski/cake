#ifndef trader_hpp
#define trader_hpp

#include <exception>
#include <string>
#include <vector>
#include <SQLAPI.h> // main SQLAPI++ header

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

class strat_param
{
public:
    virtual void mutate(void) = 0;
    virtual operator=(const strat_param &) = 0;
};

class real_param : public strat_param
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
    map<string, strat_param> parameters;
    virtual void update(void) = 0;
    virtual action_list act(void) = 0;
    const string race;
};

class mikosch_strat : public strategy
{
protected:
    SAConnection con;
public:
    mikosch_strat(void);
    mikosch_strat(const map<string, strat_param>& parameters);
};

#endif
