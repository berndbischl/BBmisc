#ifndef GETMAXCOLROWINDEX_H
#define GETMAXCOLROWINDEX_H

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP c_getMaxIndexOfRows(SEXP s_x, SEXP s_w, SEXP s_ties_method, SEXP s_na_rm);
SEXP c_getMaxIndexOfCols(SEXP s_x, SEXP s_w, SEXP s_ties_method, SEXP s_na_rm);

#endif
