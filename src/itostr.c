#include <string.h>
#include <math.h>
#include "itostr.h"

static const char base36[37] = "0123456789abcdefghijklmnopqrstuvwxyz";

SEXP itostr (SEXP x, SEXP base) {
    const R_len_t n = length(x);
    const R_len_t b = INTEGER(base)[0];
    SEXP res = PROTECT(allocVector(STRSXP, n));

    // 45 = ceiling(log( 2**64 / log(2)))
    char buffer[46];
    buffer[45] = '\0';

    for (R_len_t i = 0; i < n; i++) {
        R_len_t offset = 45;
        int xi = INTEGER(x)[i];
        do {
            buffer[--offset] = base36[xi % b];
        } while (xi /= b);

        SET_STRING_ELT(res, i, mkChar(&buffer[offset]));
    }

    UNPROTECT(1);
    return res;
}
