#include "any_special.h"

__inline bool any_special_real(SEXP x) {
  double *xi = REAL(x);
  for(int i = length(x); i--;) {
    if (ISNAN(xi[i]) || !R_FINITE(xi[i]))
      return true;
  }
  return false;
}

__inline bool any_special_complex(SEXP x) {
  for(int i = length(x); i--;) {
    if (ISNAN(COMPLEX(x)[i].r) || !R_FINITE(COMPLEX(x)[i].r) ||
        ISNAN(COMPLEX(x)[i].i) || !R_FINITE(COMPLEX(x)[i].i))
      return true;
  }
  return false;
}

bool any_special(SEXP x) {
    const int rtype = TYPEOF(x);
    bool res = false;

#ifdef DEBUG
    Rprintf("Checking Rtype %s (%i)", type2char(rtype), rtype);
#endif

    switch(rtype) {
        case LGLSXP: res = any_na_logical(x); break;
        case INTSXP: res = any_na_integer(x); break;
        case REALSXP: res = any_special_real(x); break;
        case STRSXP: res = any_na_string(x); break;
        case CPLXSXP: res = any_special_complex(x); break;
        case LISTSXP:
        case VECSXP: res = any_na_list(x); break;
        case RAWSXP: break;
        default: error("[any_special] Data type: '%s' (%i) not supported", type2char(rtype), rtype);
    }
    return res;
}

SEXP R_any_special(SEXP x) {
    return Rf_ScalarLogical(any_special(x));
}
