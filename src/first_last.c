#include "first_last.h"

SEXP c_first(SEXP x, SEXP na_omit) {
    if (!isLogical(x))
        error("Argument 'x' must be logical");
    Rboolean ignore_na = LOGICAL(na_omit)[0];
    const R_len_t n = length(x);
    int *xp = LOGICAL(x);

    for (R_len_t i = 0; i < n; i++) {
        if (xp[i] == NA_LOGICAL) {
            if (!ignore_na)
                return ScalarInteger(NA_INTEGER);
        } else if (xp[i]) {
            return ScalarInteger(i + 1);
        }
    }
    return allocVector(INTSXP, 0);
}

SEXP c_last(SEXP x, SEXP na_omit) {
    if (!isLogical(x))
        error("Argument 'x' must be logical");
    Rboolean ignore_na = LOGICAL(na_omit)[0];
    int *xp = LOGICAL(x);

    for (R_len_t i = length(x) - 1; i >= 0; i--) {
        if (xp[i] == NA_LOGICAL) {
            if (!ignore_na)
                return ScalarInteger(NA_INTEGER);
        } else if (xp[i]) {
            return ScalarInteger(i + 1);
        }
    }
    return allocVector(INTSXP, 0);
}
