#include "trader.hpp"
#include <algorithm>


trader::trader(float cash, int time_frame, const vector<float>& data)
{
  float z;
  ForwardIterator it1, it2, it3;

  if (data.size() < 2 * time_frame - 1) {
    throw trader_exception("length of data is less than twice the time frame.");
  }
  this->cash = cash;
  this->time_frame = time_frame;

  average[0] = accumulate(data.end() - 2*time_frame + 1,
			  data.end() - time_frame, 0, [](float x, float y) {
			    return x + y/time_frame;
			  });
  z = 0;
  for ((it3 = data.end() - 2*time_frame + 1,
	it1 = data.end() - time_frame,
	it2 = average.begin());
       it1 != data.end();
       it1.next(), it2.next(), it3.next()) {
    float x;
    *it2 += *it1/time_frame - z;
    z = *it3/time_frame;
    x = *it1 - *it2;
    upper_bound = max(x, upper_bound);
    lower_bound = min(x, lower_bound);
  }
  this->data = vector<float>(data.end() - time_frame, data.end());
}

trader::trader(const trader& clone)
{
  this->data = clone.data;
  this->time_frame = clone.time_frame;
  this->upper_bound = clone.upper_bound;
  this->lower_bound = clone.lower_bound;
  this->upper_margin = clone.upper_margin;
  this->lower_margin = clone.lower_margin;
  this->average = clone.average;

  this->trade = &clone.trade;
  this->mutate = &clone.mutate;
}

trader& trader::operator= (const trader& clone)
{
  return trader(clone);
}

contrararian::contrararian(float cash, int time_frame, const vector<float>& data)
  :trader(cash, time_frame, data)
{
}

float contrararian::calculate_quantity(float new_price, transaction::transaction_t type)
{
  if (type == transaction::buy)
    return cash/new_price/2;
  else
    return holding/2;
}

void contrararian::trade(float new_price, vector<transaction>& action)
{
  action->asset = asset;

  if (new_price > upper_bound * (1 + unit_upper_margin * time_frame)) {
    action.type = transaction::sell;
    action.quantity = calculate_quantity(new_price);
    action.price = new_price;
    cash += action.quantity * action.price;
    holding -= action.quantity;
  } else if (new_price < lower_bound * (1 + unit_lower_margin * time_frame)) {
    action.type = transaction::buy;
    action.quantity = calculate_quantity(new_price);
    action.price = new_price;
    holding += action.quantity;
    cash -= action.quantity * action.price;
  }

  /* Update my status */
  upper_bound = max(upper_bound, new_price);
  lower_bound = min(lower_bound, new_price);
  average.erase(average.begin());
  average.push_back(*average.end() + (new_price - data[0])/time_frame);
  data.erase(data.begin());
  data.push_back(new_price);

}
