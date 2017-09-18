#ifndef GETMAXINDEX_H
#define GETMAXINDEX_H

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

int get_max_index(double *x, R_len_t n, R_len_t step, int ties_method, Rboolean na_rm);
int get_max_index_w(double *x, double *w, R_len_t n, R_len_t step, int ties_method, Rboolean na_rm);
SEXP c_getMaxIndex(SEXP s_x, SEXP s_w, SEXP s_ties_method, SEXP s_na_rm);

#endif
