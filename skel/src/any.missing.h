#ifndef ANY_MISSING_H
#define ANY_MISSING_H

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

Rboolean any_missing(SEXP, Rboolean);
SEXP c_any_missing(SEXP, SEXP);

#endif
