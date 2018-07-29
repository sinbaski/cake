#ifndef linear_algebra_h
#define linear_algebra_h

typedef unique_ptr<gsl_matrix, decltype(&gsl_matrix_free)> GMatrix;
typedef unique_ptr<gsl_vector, decltype(&gsl_vector_free)> GVector;

GMatrix operator * (const GMatrix& A, const GMatrix& B);
GMatrix transpose(const GMatrix& A);


#endif
