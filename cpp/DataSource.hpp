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
    virtual NumericMatrix fetch(const vector<string> &symbols) = 0;
    virtual NumericMatrix fetch(const vector<int> &symbol_indices) = 0;
};

class BufferedMySQL : public DataSource
{
public:
    BufferedMySQL(string username, string passwd, string host, string database);
    NumericMatrix fetch(const vector<string> &symbols);
    NumericMatrix fetch(const vector<int> &symbol_indices);
};


#endif
