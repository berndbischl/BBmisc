#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP c_getMaxIndex(SEXP, SEXP, SEXP);
extern SEXP c_getMaxIndexOfCols(SEXP, SEXP, SEXP);
extern SEXP c_getMaxIndexOfRows(SEXP, SEXP, SEXP);
extern SEXP c_which_first(SEXP, SEXP);
extern SEXP c_which_last(SEXP, SEXP);
extern SEXP itostr(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_getMaxIndex",       (DL_FUNC) &c_getMaxIndex,       3},
    {"c_getMaxIndexOfCols", (DL_FUNC) &c_getMaxIndexOfCols, 3},
    {"c_getMaxIndexOfRows", (DL_FUNC) &c_getMaxIndexOfRows, 3},
    {"c_which_first",       (DL_FUNC) &c_which_first,       2},
    {"c_which_last",        (DL_FUNC) &c_which_last,        2},
    {"itostr",              (DL_FUNC) &itostr,              2},
    {NULL, NULL, 0}
};

void R_init_BBmisc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

