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
    string t1; // time of the least recent buffered record
    string t2; // time of the most recent buffered record
    virtual NumericMatrix fetch(const vector<int> &symbol_indices, string t1, string t2) = 0;
};

class BufferedMySQL : public DataSource
{
    BufferedMySQL(string username, string passwd, string host, string database);
    NumericMatrix fetch(const vector<int> &symbol_indices, string t1, string t2);
};


#endif
