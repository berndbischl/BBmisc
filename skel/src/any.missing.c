#include "any.missing.h"

SEXP c_any_missing(SEXP x) {
    return ScalarLogical(any_missing(x));
}

Rboolean any_missing(SEXP x) {
    if (! (isVectorAtomic(x) || isNewList(x)))
        error("Object of type '%s' not supported", type2char(TYPEOF(x)));

    const R_len_t n = length(x);
    if (n == 0)
        return FALSE;

    switch(TYPEOF(x)) {
        case LGLSXP:
            for (R_len_t i = 0; i < n; i++) {
                if (LOGICAL(x)[i] == NA_LOGICAL)
                    return TRUE;
            }
            break;
        case INTSXP:
            for (R_len_t i = 0; i < n; i++) {
                if (INTEGER(x)[i] == NA_INTEGER)
                    return TRUE;
            }
            break;
        case REALSXP:
            for (R_len_t i = 0; i < n; i++) {
                if (ISNAN(REAL(x)[i]))
                    return TRUE;
            }
            break;
        case CPLXSXP:
            for (R_len_t i = 0; i < n; i++) {
                if (ISNAN(COMPLEX(x)[i].r) || ISNAN(COMPLEX(x)[i].i))
                    return TRUE;
            }
            break;
        case STRSXP:
            for (R_len_t i = 0; i < n; i++) {
                if (STRING_ELT(x, i) == NA_STRING)
                    return TRUE;
            }
            break;
        case VECSXP:
            for (R_len_t i = 0; i < n; i++) {
                if (isNull(VECTOR_ELT(x, i)))
                    return TRUE;
            }
            break;
        case RAWSXP: /* yep, also atomic */
            return FALSE;
        default:
            error("Internal error: unsupported input type");
        }
    return FALSE;
}
