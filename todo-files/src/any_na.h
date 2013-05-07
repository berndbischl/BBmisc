#ifndef ANY_NA_H
#define ANY_NA_H

#include <R_ext/Arith.h>
#include <Rinternals.h>
#include <stdbool.h>
#include "any_na.h"

//#define DEBUG

bool any_na_logical(SEXP x);
bool any_na_integer(SEXP x);
bool any_na_real(SEXP x);
bool any_na_string(SEXP x);
bool any_na_complex(SEXP x);
bool any_na_list(SEXP x);
bool any_na(SEXP x);
SEXP R_any_na(SEXP x);

#endif
