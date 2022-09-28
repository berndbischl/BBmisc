#include "getMaxColRowIndex.h"
#include "getMaxIndex.h"
#include "macros.h"

SEXP c_getMaxIndexOfRows(SEXP s_x, SEXP s_w, SEXP s_ties_method, SEXP s_na_rm) {
  int ties_method = asInteger(s_ties_method);
  Rboolean na_rm = asLogical(s_na_rm);
  UNPACK_REAL_MATRIX(s_x, x, nrow_x, ncol_x);
  SEXP s_ret = PROTECT(allocVector(INTSXP, nrow_x));
  int* ret = INTEGER(s_ret);
  GetRNGstate();
  UNPACK_REAL_VECTOR(s_w, w, len_w);
  // call unweighed version if s_w is numeric(0)
  if (len_w == 0) {
    for (R_len_t i = 0; i < nrow_x; i++)
        ret[i] = get_max_index(x + i, ncol_x, nrow_x, ties_method, na_rm);
  } else {
    double *buf = (double *) malloc(ncol_x * sizeof(double));
    for (R_len_t i = 0; i < nrow_x; i++)
      ret[i] = get_max_index_w(x + i, w, buf, ncol_x, nrow_x, ties_method, na_rm);
    free(buf);
  }
  PutRNGstate();
  UNPROTECT(1); /* s_ret */
  return s_ret;
}

SEXP c_getMaxIndexOfCols(SEXP s_x, SEXP s_w, SEXP s_ties_method, SEXP s_na_rm) {
  int ties_method = asInteger(s_ties_method);
  Rboolean na_rm = asInteger(s_na_rm);
  UNPACK_REAL_MATRIX(s_x, x, nrow_x, ncol_x);
  SEXP s_ret = PROTECT(allocVector(INTSXP, ncol_x));
  int* ret = INTEGER(s_ret);
  GetRNGstate();
  UNPACK_REAL_VECTOR(s_w, w, len_w);
  // call unweighed version if s_w is numeric(0)
  if (len_w == 0) {
    for (R_len_t i = 0; i < ncol_x; ++i)
      ret[i] = get_max_index(x + i*nrow_x, nrow_x, 1, ties_method, na_rm);
  } else {
    double *buf = (double *) malloc(nrow_x * sizeof(double));
    for (R_len_t i = 0; i < ncol_x; ++i)
      ret[i] = get_max_index_w(x + i*nrow_x, w, buf, nrow_x, 1, ties_method, na_rm);
    free(buf);
  }
  PutRNGstate();
  UNPROTECT(1); /* s_ret */
  return s_ret;
}
