#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#define UNPACK_REAL_MATRIX(S, D, N, K) \
  double *D = REAL(S); \
  const R_len_t N = nrows(S); \
  const R_len_t K = ncols(S);

int get_max_index(double *x, size_t n, size_t stride, int ties_method, int na_rm);

SEXP c_getMaxIndexOfRows(SEXP s_x, SEXP s_ties_method, SEXP s_na_rm) {
  int ties_method = asInteger(s_ties_method);
  int na_rm = asInteger(s_na_rm);
  UNPACK_REAL_MATRIX(s_x, x, nrow_x, ncol_x);
  SEXP s_ret = allocVector(INTSXP, nrow_x);
  PROTECT(s_ret);
  int* ret = INTEGER(s_ret);
  GetRNGstate();
  for (R_len_t i = 0; i < nrow_x; ++i) {
    ret[i] = get_max_index(x + i, ncol_x, nrow_x, ties_method, na_rm);
  }
  PutRNGstate();
  UNPROTECT(1); /* s_ret */
  return s_ret;
}

SEXP c_getMaxIndexOfCols(SEXP s_x, SEXP s_ties_method, SEXP s_na_rm) {
  int ties_method = asInteger(s_ties_method);
  int na_rm = asInteger(s_na_rm);
  UNPACK_REAL_MATRIX(s_x, x, nrow_x, ncol_x);
  SEXP s_ret = allocVector(INTSXP, ncol_x);
  PROTECT(s_ret);
  int* ret = INTEGER(s_ret);
  GetRNGstate();
  for (R_len_t i = 0; i < ncol_x; ++i) {
    ret[i] = get_max_index(x + i*nrow_x, nrow_x, 1, ties_method, na_rm);
  }
  PutRNGstate();
  UNPROTECT(1); /* s_ret */
  return s_ret;
}
