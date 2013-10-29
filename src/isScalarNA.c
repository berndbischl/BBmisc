#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP c_isScalarNA(SEXP x) {
    if (length(x) != 1)
        return ScalarLogical(FALSE);

    switch(TYPEOF(x)) {
        case LGLSXP:
            return ScalarLogical(LOGICAL(x)[0] == NA_LOGICAL);
        case INTSXP:
            return ScalarLogical(INTEGER(x)[0] == NA_INTEGER);
        case REALSXP:
            return ScalarLogical(ISNAN(REAL(x)[0]));
        case CPLXSXP:
            return ScalarLogical(ISNAN(COMPLEX(x)[0].r) || ISNAN(COMPLEX(x)[0].i));
        case STRSXP:
            return ScalarLogical(STRING_ELT(x, 0) == NA_STRING);
    }

    return ScalarLogical(FALSE);
}
