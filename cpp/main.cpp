#include <vector>
#include <Rinternals.h>
#include <Rembedded.h>
#include <memory.h>

using namespace std;

/**
* Invokes the command source("foo.R").
*/
void source(const char *name)
{
    SEXP e;
 
    PROTECT(e = lang2(install("source"), mkString(name)));
    R_tryEval(e, R_GlobalEnv, NULL);
    UNPROTECT(1);
}

/**
* Invokes the command source("foo.R").
*/
void require(const char *name)
{
    SEXP e;
 
    PROTECT(e = lang2(install("require"), mkString(name)));
    R_tryEval(e, R_GlobalEnv, NULL);
    UNPROTECT(1);
}


int main(int argc, char *argv[])
{
    int A[] = { 1, 2, 3, 4, 5 };
    for (int i = 0; i < 5; i++) {
	printf("%d, ", A[i]);
    }
    printf("\n");
    
    // source("~/cake/libeix.r");
    // require("fGarch");
    SEXP arg;
    PROTECT(arg = allocVector(INTSXP, 5));
    // copy(A.begin(), A.end(), INTEGER(arg));
    memcpy(INTEGER(arg), A, 5 * sizeof(int));

    SEXP funcall;
    PROTECT(funcall = lang2(install("sum"), arg));

    int errcode;
    SEXP ret = R_tryEval(funcall, R_GlobalEnv, &errcode);

    if (errcode) {
    	double *val = REAL(ret);
    	for (int i = 0; i < LENGTH(ret); i++) {
    	    printf("%.4f, ", val[i]);
    	}
    	printf("\n");
    }
    UNPROTECT(2);
    Rf_endEmbeddedR(0);

    return 0;
}

