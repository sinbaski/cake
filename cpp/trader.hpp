#ifndef trader_hpp
#define trader_hpp

#include <exception>
#include <vector>

using namespace std;

// class newdata
// {
// public:
//   const char *asset;
//   float price;
// }

class trader_exception : public exception
{
private:
	const char *message;

public:
	trader_exception(const char *message) {
		this->message = message;
	};

	virtual const char* what() const throw() {
		return message;
	};
};

class transaction
{
public:
	enum transaction_t {buy, sell};

	const char *asset;
	float quantity;
	float price;
	transaction_t type;
};

/**
 * Each trader trades only one asset.
 */
class trader
{
protected:
	float holding = 0;
	unsigned int time_frame;
	float upper_bound = 0;
	float lower_bound = 0;
	float unit_upper_margin = 0;
	float unit_lower_margin = 0;
	vector<float> data;
	vector<float> average;

public:
	const char *asset;
	float cash = 1;
	virtual void trade(float new_price, transaction& action) = 0;
	virtual void mutate(void) = 0;
	virtual float computeVaR(void) = 0;

	trader(float cash, int time_frame, const vector<float>& data) throw();
	trader(const trader& clone);
	trader& operator= (const trader& x);
	~trader(void);

	float get_cash(void) {
		return cash;
	};

	float get_holding(void) {
		return holding;
	}
};

class contrararian : public trader
{
private:

public:
	contrararian(float cash, int time_frame, const vector<float>& data)
		throw();
	virtual float calculate_quantity(
		float new_price,
		transaction::transaction_t type);
	virtual void trade(float new_price, transaction& action);
};

#endif
