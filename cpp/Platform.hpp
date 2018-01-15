#ifndef DataSource_hpp
#define DataSource_hpp

#include "DataSource.hpp"

using namespace std;
using namespace Rcpp;

class Platform
{
private:
    DataSource *data_source;
public:
    RInside &prepared_r_instance(void);
};

#endif
