#include "anyMissing.h"

SEXP c_anyMissing(SEXP x, SEXP inf_as_missing) {
    if (!isLogical(inf_as_missing) || length(inf_as_missing) != 1 || LOGICAL(inf_as_missing)[0] == NA_LOGICAL)
        error("Argument 'inf.as.missing' must be a scalar logical");
    return ScalarLogical(anyMissing(x, LOGICAL(inf_as_missing)[0]));
}

Rboolean anyMissing(SEXP x, Rboolean inf_as_missing) {
    const R_len_t n = length(x);

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
            if (inf_as_missing) {
                for (R_len_t i = 0; i < n; i++) {
                    if (!R_FINITE(REAL(x)[i]))
                        return TRUE;
                }
            } else {
                for (R_len_t i = 0; i < n; i++) {
                    if (ISNAN(REAL(x)[i]))
                        return TRUE;
                }
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
        case NILSXP:
            return FALSE;
        case VECSXP:
            for (R_len_t i = 0; i < n; i++) {
                if (isNull(VECTOR_ELT(x, i)))
                    return TRUE;
            }
            break;
        case RAWSXP:
            return FALSE;
        default:
            error("Object of type '%s' not supported", type2char(TYPEOF(x)));
        }

    return FALSE;
}
