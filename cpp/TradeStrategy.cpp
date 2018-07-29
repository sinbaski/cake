#include <gsl/gsl_vector.h>
#include "TradeStrategy.hpp"
#include "LinearAlgebra.hpp"
#include "timeseries.hpp"

vector<double> ts_strategy::trade(const gsl_matrix *data, size_t valid_nrow)
{
    if (lookback == 0) return vector<double>;

    unique_ptr<gsl_eigen_symmv_workspace, void(*)(gsl_eigen_symmv_workspace *)>
	workspace(gsl_eigen_symmv_alloc, gsl_eigen_symmv_free);

    GMatrix X(gsl_matrix_alloc(lookback, data->size2), gsl_matrix_free);

    size_t n = valid_nrow - lookback;
    auto view = gsl_matrix_const_submatrix(
	data, n, 0, lookback, data->size2
	);
    
    gsl_matrix_memcpy(X.get(), &view.matrix);
    auto C = transpose(X) * X / n;
    GVector evals(gsl_vector_alloc(C->size1), gsl_vector_free);
    GMatrix evecs(gsl_matrix_alloc(C->size1, C->size1), gsl_matrix_free);
    gsl_eigen_symmv(C.get(), evals.get(), evecs.get(), workspace.get());
    gsl_eigen_symmv_sort(evals.get(), evecs.get(), GSL_EIGEN_SORT_VAL_DESC);
    auto Y = X * evecs;
    vector<array<double, 2>> predictions;
    for (size_t i = 0; i < evals->size && gsl_vector_get(evals.get(), i) > 0; i++) {
	auto *factor = &gsl_matrix_column(Y.get(), i).vector;
	
	ARIMA<double*> model;
	double *p = gsl_vector_const_ptr(factor, 0);
	model.fit_arma1(p, p + factor->size, "normal");
    }
    return vector<double>;
}
