#include <stdio.h>
#include <iostream>
#include <vector>
#include <cmath>
#include <string>
#include <random>
#include <algorithm>
#include <mysql.h>
#include <gsl/gsl_cdf.h>
#include <numeric>


using namespace std;
random_device randev;

// #include <RInside.h>

// int main(int argc, char *argv[])
// {
//     RInside R(argc, argv);
//     R["txt"] = "Hello World!\n";
//     R.parseEvalQ("cat(txt)");
//     return 0;
// }

class EuropeanCallOption
{
public:
    static double fair_price(unsigned int T, double K, double r, const vector<double> &prices);
};

double EuropeanCallOption::fair_price(
    unsigned int T, double K, double r, const vector<double> &prices)
{
    vector<double> S(prices);
    transform(S.begin(), S.end(), S.begin(),
	      [](double x) {return log(x);});
    adjacent_difference(S.begin(), S.end(), S.begin());
    S.erase(S.begin());
    size_t n = S.size();
    double avg = accumulate(
	S.begin(), S.end(), (double)0,
	[=](double y, double x) {
	    return y + x/n;
	});
    double sigma = sqrt(accumulate(
	S.begin(), S.end(), (double)0,
	[=](double y, double x) {
	    return y + (x - avg)*(x - avg)/(n-1);
	}));
    double d[2];
    unsigned int t = prices.size();
    d[0] = log(prices.back()/K) + (r + sigma * sigma/2) * (T - t);
    d[0] /= sigma * sqrt(T - t);
    d[1] = d[0] - sigma * sqrt(T - t);
    return gsl_cdf_ugaussian_P(d[0]) * prices.back() -
      gsl_cdf_ugaussian_P(d[1]) * K * exp(-r * (T - prices.size()));
}

class BrownianMotion
{
public:
    static vector<double> simulate(unsigned int n);
};

class GeoBrownianMotion
{
public:
    static vector<double> simulate(unsigned int n, double mu, double sigma);
};

vector<double> BrownianMotion::simulate(unsigned int n)
{
    normal_distribution<double> dist(0, 1);
    vector<double> V(n);
    generate(V.begin(), V.end(),
    	     [&]()->double {
    		 dist(randev);
    	     });
    // for_each(V.begin(), V.end(), [](double x) {printf("%e ", x);});
    // cout << endl;
    partial_sum(V.begin(), V.end(), V.begin());
    return V;
}

vector<double> GeoBrownianMotion::simulate(
    unsigned int n, double mu, double sigma)
{
    vector<double> B = BrownianMotion::simulate(n);
    vector<unsigned int> tm(n);
    unsigned int i = 1;
    generate(tm.begin(), tm.end(), [&i]() -> double {
	    return i++;
	});
    transform(
	B.begin(), B.end(), tm.begin(), B.begin(),
	[=](double x, unsigned int i) {
	    return exp(sigma * x + (mu - sigma * sigma/2) * i);
	});
    B.insert(B.begin(), 1);
    return B;
}


int main(int argc, char *argv[])
{
    MYSQL *conn;
    conn = mysql_init(NULL);
    mysql_real_connect(conn, "127.0.0.1", "sinbaski", "q1w2e3r4", "market",
		       0, NULL, 0);
    int err = mysql_query(
	conn,
	"select closing from spy_daily order by tm desc limit 252"
	);
    if (err) {
	cout <<  mysql_error(conn) << endl;
	return 0;
    }
    MYSQL_RES *res = mysql_store_result(conn);
    // vector<double> S(GeoBrownianMotion::simulate(504, 5.0e-4, 1.0e-3));
    vector<double> S(mysql_num_rows(res));
    generate(S.rbegin(), S.rend(), [res]() -> double {
	    MYSQL_ROW row = mysql_fetch_row(res);
	    return stod(row[0]);
	});
    mysql_free_result(res);
    mysql_close(conn);
    double interest_rate = 1.0e-4;
    double s = EuropeanCallOption::fair_price(
    	S.size() + 10, S.back() * 1.05, interest_rate, S
    	);
    cout << s << endl;
    
    return 0;
}

