#include <exception>

#include "LinearAlgebra.hpp"


GMatrix operator * (const GMatrix& A, const GMatrix& B)
{
    if (A->size2 != B->size1)
	throw exception("Dimensions mismatch.");
    GMatrix C(gsl_matrix_alloc(A->size1, B->size2), gsl_matrix_free);

#pragma omp parallel for
    for (size_t i = 0; i < C->size1; i++) {
	for (size_t j = 0; j < C->size2; j++) {
	    double x = 0;
	    for (size_t k = 0; k < A->size2; k++) {
		x += gsl_matrix_get(A.get(), i, k) * gsl_matrix_get(B.get(), k, j);
	    }
	    gsl_matrix_set(C.get(), i, j, x);
	}
    }
    return C;
}

GMatrix transpose (const GMatrix& A)
{
    GMatrix M(gsl_matrix_alloc(A->size2, A->size1), gsl_matrix_free);
    gsl_matrix_transpose_memcpy(M.get(), A.get());
    return M;
}


GMatrix operator / (const GMatrix &A, double a)
{
    GMatrix B(gsl_matrix_alloc(A->size1, A->size2), gsl_matrix_free);
    gsl_matrix_scale(B.get(), 1/a);
    return B;
}
