#include "first_last.h"

SEXP c_first(SEXP x, SEXP na_omit) {
    if (!isLogical(x))
        error("Argument 'x' must be logical");

    const R_len_t n = length(x);
    for (R_len_t i = 0; i < n; i++) {
        if (LOGICAL(x)[i] == NA_LOGICAL) {
            if (!asLogical(na_omit))
                return ScalarInteger(NA_INTEGER);
        } else if (LOGICAL(x)[i]) {
            return ScalarInteger(i + 1);
        }
    }
    return allocVector(INTSXP, 0);
}

SEXP c_last(SEXP x, SEXP na_omit) {
    if (!isLogical(x))
        error("Argument 'x' must be logical");

    const R_len_t n = length(x);
    for (R_len_t i = n - 1; i >= 0; i--) {
        if (LOGICAL(x)[i] == NA_LOGICAL) {
            if (!asLogical(na_omit))
                return ScalarInteger(NA_INTEGER);
        } else if (LOGICAL(x)[i]) {
            return ScalarInteger(i + 1);
        }
    }
    return allocVector(INTSXP, 0);
}
