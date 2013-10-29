#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP c_isScalarValue(SEXP x, SEXP s_na_ok, SEXP s_null_ok) {
    if (!isLogical(s_na_ok) || length(s_na_ok) != 1)
        error("Argument 'na.ok' must be a scalar logical");
    if (!isLogical(s_na_ok) || length(s_na_ok) != 1)
        error("Argument 'null.ok' must be a scalar logical");

    Rboolean na_ok = asLogical(s_na_ok);
    switch(TYPEOF(x)) {
        case NILSXP:
            return ScalarLogical(asLogical(s_null_ok));
        case LGLSXP:
            return ScalarLogical(length(x) == 1 && (na_ok || LOGICAL(x)[0] != NA_LOGICAL));
        case INTSXP:
            return ScalarLogical(length(x) == 1 && (na_ok || INTEGER(x)[0] != NA_INTEGER));
        case REALSXP:
            return ScalarLogical(length(x) == 1 && (na_ok || !ISNAN(REAL(x)[0])));
        case CPLXSXP:
            return ScalarLogical(length(x) == 1 && (na_ok || (!ISNAN(COMPLEX(x)[0].r) && !ISNAN(COMPLEX(x)[0].i))));
        case STRSXP:
            return ScalarLogical(length(x) == 1 && (na_ok || STRING_ELT(x, 0) != NA_STRING));
    }

    return ScalarLogical(FALSE);
}
