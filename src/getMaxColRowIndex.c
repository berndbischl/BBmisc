#include "getMaxColRowIndex.h"
#include "getMaxIndex.h"
#include "macros.h"

SEXP c_getMaxIndexOfRows(SEXP s_x, SEXP s_ties_method, SEXP s_na_rm) {
  int ties_method = asInteger(s_ties_method);
  Rboolean na_rm = asLogical(s_na_rm);
  UNPACK_REAL_MATRIX(s_x, x, nrow_x, ncol_x);
  SEXP s_ret = PROTECT(allocVector(INTSXP, nrow_x));
  int* ret = INTEGER(s_ret);
  GetRNGstate();
  for (R_len_t i = 0; i < nrow_x; i++) {
    ret[i] = get_max_index(x + i, ncol_x, nrow_x, ties_method, na_rm);
  }
  PutRNGstate();
  UNPROTECT(1); /* s_ret */
  return s_ret;
}

SEXP c_getMaxIndexOfCols(SEXP s_x, SEXP s_ties_method, SEXP s_na_rm) {
  int ties_method = asInteger(s_ties_method);
  Rboolean na_rm = asInteger(s_na_rm);
  UNPACK_REAL_MATRIX(s_x, x, nrow_x, ncol_x);
  SEXP s_ret = PROTECT(allocVector(INTSXP, ncol_x));
  int* ret = INTEGER(s_ret);
  GetRNGstate();
  for (R_len_t i = 0; i < ncol_x; ++i) {
    ret[i] = get_max_index(x + i*nrow_x, nrow_x, 1, ties_method, na_rm);
  }
  PutRNGstate();
  UNPROTECT(1); /* s_ret */
  return s_ret;
}
