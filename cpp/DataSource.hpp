#ifndef DataSource_hpp
#define DataSource_hpp

#include <exception>
#include <string>
#include <vector>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

class DataSource
{
public:
    NumericMatrix fetch(const vector<string> &symbols);
    NumericMatrix fetch(const vector<int> &symbols);
};


#endif
