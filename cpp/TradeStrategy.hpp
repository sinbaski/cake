#ifndef trade_strategy_h
#include <unordered_map>
#include <gsl/gsl_matrix.h>

class TradeStrategy
{
public:
    unordered_map<string, double> params;
    vector<double> trade(
	shared_ptr<const gsl_matrix, decltype(&gsl_matrix_free)> data,
	size_t valid_size) = 0;
};

class ts_strategy : public TradeStrategy
{
public:
    size_t lookback;
    
    vector<double> trade(const gsl_matrix *data, size_t valid_nrow);

    ts_strategy() : lookback(0) {
    };

    ts_strategy(size_t tm) : lookback(tm) {
    };

};



#define trade_strategy_h
#endif
