#ifndef ANY_SPECIAL_H
#define ANY_SPECIAL_H

#include <R_ext/Arith.h>
#include <Rinternals.h>
#include <stdbool.h>
#include "any_na.h"

bool any_special(SEXP x);
SEXP R_any_special(SEXP x);

#endif
