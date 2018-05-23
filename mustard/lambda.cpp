#include <vector>
#include <unordered_map>
#include <mysql.h>
#include <string>
#include <cmath>
#include <cassert>
#include <stdio.h>
#include <cstdlib>
#include <algorithm>
#include <numeric>
#include <gsl/gsl_min.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_multifit.h>

using namespace std;

class TeamPair
{
public:
    int recorded_matches;
    double lambda;
    string teams[2];
    TeamPair(const string &t1, const string &t2)
	: recorded_matches(-1), lambda(-1), teams{t1, t2}{
    };
};

class LinearModel
{
public:
    gsl_vector *beta;
    gsl_matrix *beta_cov;

    LinearModel() : beta(NULL), beta_cov(NULL) {
    };

    void clean() {
	if (beta != NULL) {
	    gsl_vector_free(beta);
	    beta = NULL;
	}
	if (beta_cov != NULL) {
	    gsl_matrix_free(beta_cov);
	    beta_cov = NULL;
	}
    };

    double predict(const vector<double> &X) {
	assert(X.size() == beta->size);
	double resp = 0;
	for (size_t i = 0; i < X.size(); i++) {
	    resp += X[i] * gsl_vector_get(beta, i);
	}
	return resp;
    };

    void fit(const gsl_vector *Y, const gsl_matrix *X) {
	clean();
	beta = gsl_vector_alloc(X->size2);
	beta_cov = gsl_matrix_alloc(X->size2, X->size2);

	double chi2;
	gsl_multifit_linear_workspace *work =
	    gsl_multifit_linear_alloc(X->size1, 2);
	gsl_multifit_linear(X, Y, beta, beta_cov, &chi2, work);
	gsl_multifit_linear_free(work);
    };

    void fit(MYSQL_RES *res, int y_idx, const vector<int> &x_idx) {
	clean();
	size_t n = mysql_num_rows(res);
	gsl_matrix *X = gsl_matrix_alloc(n, x_idx.size());
	gsl_vector *Y = gsl_vector_alloc(n);
	for (size_t i = 0; i < n; i++) {
	    MYSQL_ROW row = mysql_fetch_row(res);
	    gsl_vector_set(Y, i, atof(row[y_idx]));
	    for (auto k = x_idx.cbegin(); k != x_idx.cend(); k++) {
		gsl_matrix_set(X, i, *k, atof(row[*k]));
	    }
	}
	mysql_free_result(res);

	fit(Y, X);
	gsl_matrix_free(X);
	gsl_vector_free(Y);
    };

    LinearModel(const gsl_vector *Y, const gsl_matrix *X)
	: beta(NULL), beta_cov(NULL) {
	fit(Y, X);
    };

    LinearModel(MYSQL_RES *res, int y_idx, const vector<int> &x_idx)
	: beta(NULL), beta_cov(NULL) {
	fit(res, y_idx, x_idx);
    };
    
    ~LinearModel() {
	clean();
    }
};

unordered_map<string, TeamPair> teams;

double poisson_nllh(double lambda, void *observations)
{
    auto *obs = (vector<unsigned int> *)observations;
    return accumulate(
	obs->cbegin(), obs->cend(), 0.0,
	[lambda](double s, unsigned int x) {
	    return s - log(gsl_ran_poisson_pdf(x, lambda));
	});
}

int main(int argc, char *argv[])
{
    MYSQL *conn;
    conn = mysql_init(NULL);
    mysql_real_connect(conn, "127.0.0.1", "sinbaski", "q1w2e3r4",
		       "market", 0, NULL, 0);
    string qstr(
	"select home, away, N from ("
	"       select home, away, count(*) as N from matches_train "
	"       group by home, away "
	") as tbl where N >=");
    qstr += argv[1];
    mysql_query(conn, qstr.c_str());
    MYSQL_RES *res = mysql_store_result(conn);
    unsigned int L = mysql_num_rows(res);
    gsl_min_fminimizer *me = gsl_min_fminimizer_alloc(gsl_min_fminimizer_brent);

    // estimate lambda[home][away] for those pairs that have directly
    // played against each other.
    for (unsigned int c = 0; c < L; c++) {
	MYSQL_ROW row = mysql_fetch_row(res);

	mysql_query(
	    conn,
	    string("select home_corners from matches_train "
	    "where home ='" + string(row[0]) + "' and away = '" +
		   string(row[1]) + "' ").c_str()
	    );
	MYSQL_RES *res2 = mysql_store_result(conn);
	vector<unsigned int> home_corners(mysql_num_rows(res2));
	generate(
	    home_corners.begin(), home_corners.end(),
	    [res2]() -> unsigned int {
		MYSQL_ROW r = mysql_fetch_row(res2);
		return stoul(r[0]);
	    });
	mysql_free_result(res2);

	// maximum likelihood estimate of the lambda[home][away]
	gsl_function func;
	func.function = &poisson_nllh;
	func.params = &home_corners;
	double lambda_init = accumulate(
	    home_corners.cbegin(),
	    home_corners.cend(), 0
	    )/(double)home_corners.size();

	double x, y, a;
	a = poisson_nllh(lambda_init, &home_corners);
	for (x = lambda_init; poisson_nllh(x, &home_corners) <= a; x/=2);
	for (y = lambda_init; poisson_nllh(y, &home_corners) <= a; y*=2);
	gsl_min_fminimizer_set(me, &func, lambda_init, x, y);
	int status;
	do {
	    gsl_min_fminimizer_iterate(me);
	    status = gsl_min_test_interval(
		gsl_min_fminimizer_x_lower(me),
		gsl_min_fminimizer_x_upper(me),
		1.0e-3, 0.0
		);
	} while(status == GSL_CONTINUE);
	TeamPair tp(row[0], row[1]);
	tp.lambda = gsl_min_fminimizer_x_minimum(me);
	tp.recorded_matches = stoi(row[2]);
	teams.emplace(string(row[0])+ "-" + row[1], tp);
    }
    gsl_min_fminimizer_free(me);
    mysql_free_result(res);

    mysql_query(
	conn, "drop table if exists home_corners_intensity"
	);
    mysql_query(
	conn,
	"create table home_corners_intensity ("
	"home varchar(8), "
	"away varchar(8), "
	"intensity decimal(8, 4), "
	"order tinyint, "
	"primary key (home, away))"
	);
    for (auto i = teams.cbegin(); i != teams.cend(); i=next(i)) {
	string str("insert into home_corners_intensity values (");
	str += "'" + i->second.teams[0] + "', '" + i->second.teams[1] + "', " +
	    to_string(i->second.lambda) + ", 0)";
	mysql_query(conn, str.c_str());
    }

    // estimate lambda[home][away] for those pairs that haven't
    // played against each other directly but have played against
    // the same opponents.
    mysql_query(
	conn,
	"select A.home, A.away, A.intensity, "
	"avg(B.intensity), avg(C.intensity) "
	"from home_corners_intensity as A "
	"join home_corners_intensity as B "
	"join home_corners_intensity as C "
	"on A.home = B.home and "
	"B.away = C.home and "
	"C.away = A.away "
	"group by A.home, A.away"
	);
    res = mysql_store_result(conn);
    LinearModel model(res, 2, vector<int>{3,4});
    mysql_free_result(res);
    do {
	mysql_query(
	    conn,
	    "select A.home, A.away, count(*), "
	    "avg(B.intensity), avg(C.intensity) "
	    "from matches_test as A "
	    "join home_corners_intensity as B "
	    "join home_corners_intensity as C "
	    "on A.home = B.home "
	    "and B.away = C.home "
	    "and C.away = A.away "
	    "where (A.home, A.away) not in ("
	    "   select home, away from home_corners_intensity"
	    ") "
	    "group by A.home, A.away "
	    "order by count(*) desc "
	    "limit 1"
	    );
	res = mysql_store_result(conn);
	if (mysql_num_rows(res) == 0) break;

	MYSQL_ROW row = mysql_fetch_row(res);
	if (stoi(row[2]) == 0) break;
	double x = model.predict(vector<double>{atof(row[3]), atof(row[4])});
	string stmt = string("insert into home_corners_intensity values (") +
	    "'" + row[0] + "', '" + row[1] + "', " + to_string(x) + ", 1)";
	mysql_free_result(res);
	mysql_query(conn, stmt.c_str());
    } while(true);
    mysql_free_result(res);

    // estimate lambda[home][away] for those pairs that haven't
    // played against each other or against the same opponents.
    mysql_query(
	conn,
	"select A.home, A.away, A.intensity, B.itn, C.itn "
	"from home_corners_intensity as A "
	"join ( "
	"     select home, avg(intensity) as itn "
	"     from home_corners_intensity "
	"     group by home "
	") as B "
	"join ( "
	"     select away, avg(intensity) as itn "
	"     from home_corners_intensity "
	"     group by away "
	") as C "
	"on A.home = B.home and A.away = C.away "
	);
    res = mysql_store_result(conn);
    model.fit(res, 2, vector<int>{3,4});
    mysql_free_result(res);

    do {
	mysql_query(
	    conn,
	    "select A.home, A.away, B.itn, C.itn "
	    "from ( "
	    "     select home, away from matches_test "
	    "     where (home, away) not in ("
	    "          select home, away from home_corners_intensity "
	    ")) as A "
	    "join ( "
	    "     select home, avg(intensity) as itn "
	    "     from home_corners_intensity "
	    "     group by home "
	    ") as B "
	    "join ( "
	    "     select away, avg(intensity) as itn "
	    "     from home_corners_intensity "
	    "     group by away "
	    ") as C "
	    "on A.home = B.home and A.away = C.away "
	    );
    } while(true);
    mysql_close(conn);
}
