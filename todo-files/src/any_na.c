#include "any_na.h"

__inline bool is_na_scalar(SEXP x) {
    const int rtype = TYPEOF(x);
    bool res = false;
    switch(rtype) {
        case LGLSXP: res = LOGICAL(x)[0] == NA_LOGICAL; break;
        case INTSXP: res = INTEGER(x)[0] == NA_INTEGER; break;
        case REALSXP: res = ISNA(REAL(x)[0]); break;
        case STRSXP: res = STRING_ELT(x, 0) == NA_STRING; break;
        case CPLXSXP: res = ISNA(COMPLEX(x)[0].r) || ISNA(COMPLEX(x)[0].i); break;
        case RAWSXP: break;
        default: error("[is_na_scalar] Data type: '%s' (%i) not supported", type2char(rtype), rtype);
    }
    return res;
}

__inline bool any_na_logical(SEXP x) {
    int *xi = LOGICAL(x);
    for (int i = length(x); i--;) {
        if (xi[i] == NA_LOGICAL)
            return true;
    }
    return false;
}

__inline bool any_na_integer(SEXP x) {
    int *xi = INTEGER(x);
    for (int i = length(x); i--;) {
        if (xi[i] == NA_INTEGER)
            return true;
    }
    return false;
}

__inline bool any_na_real(SEXP x) {
    double *xi = REAL(x);
    for(int i = length(x); i--;) {
        if (ISNA(xi[i]))
            return true;
    }
    return false;
}

__inline bool any_na_string(SEXP x) {
    for(int i = length(x); i--;) {
        if (STRING_ELT(x, i) == NA_STRING)
            return true;
    }
    return false;
}

__inline bool any_na_complex(SEXP x) {
    for(int i = length(x); i--;) {
        // ISNAN is right, what is a little bit strange ... whatever
        if (ISNAN(COMPLEX(x)[i].r) || ISNAN(COMPLEX(x)[i].i))
            return true;
    }
    return false;
}

__inline bool any_na_list_recursive(SEXP x) {
    for(int i = length(x); i--;) {
        if(any_na(VECTOR_ELT(x, i)))
            return true;
    }
    return false;
}

__inline bool any_na_list(SEXP x) {
    SEXP xi;
    for(int i = length(x); i--;) {
        xi = VECTOR_ELT(x, i);
        if(length(xi) == 1 && is_na_scalar(xi))
            return true;
    }
    return false;
}

__inline bool any_na(SEXP x) {
    const int rtype = TYPEOF(x);
    bool res = false;

#ifdef DEBUG
    Rprintf("Checking Rtype %s (%i)", type2char(rtype), rtype);
#endif

    switch(rtype) {
        case LGLSXP: res = any_na_logical(x); break;
        case INTSXP: res = any_na_integer(x); break;
        case REALSXP: res = any_na_real(x); break;
        case STRSXP: res = any_na_string(x); break;
        case CPLXSXP: res = any_na_complex(x); break;
        case LISTSXP:
        case VECSXP: res = any_na_list(x); break;
        case RAWSXP: break;
        default: error("[any_na] Data type: '%s' (%i) not supported", type2char(rtype), rtype);
    }
    return res;
}

SEXP R_any_na(SEXP x) {
    return Rf_ScalarLogical(any_na(x));
}
