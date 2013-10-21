#ifndef ANYMISSING_H
#define ANYMISSING_H

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

Rboolean anyMissing(SEXP, Rboolean);
SEXP c_anyMissing(SEXP, SEXP);

#endif
