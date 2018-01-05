#include <Rcpp.h>
#include "strategy.hpp"

using namespace Rcpp;

mikosch_strat::mikosch_strat(void)
    : race(string("mikosch"))
{
}

mikosch_strat::mikosch_strat(const map<string, variable_param>& parameters)
    :parameters(parameters), race(string("mikosch"))
{
}


action_list mikosch_strat::act(void)
{
    
}
