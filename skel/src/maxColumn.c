#include <R.h>
#include <Rinternals.h>

#define CHECK_ARG_IS_REAL_MATRIX(A)                             \
    if (!isReal(A) || !isMatrix(A))                             \
        error("Argument '" #A "' is not a real matrix.");

#define UNPACK_REAL_MATRIX(S, D, N, K)          \
    CHECK_ARG_IS_REAL_MATRIX(S);                \
    double *D = REAL(S);                        \
    const R_len_t N = nrows(S);                 \
    const R_len_t K = ncols(S);

static int randomized_numeric_max(double *x, size_t n, size_t stride) {
    size_t i;
    int max_index, number_of_ties = 0;
    double max_value = DBL_MIN;
    
    for (i = 0; i < n; ++i) {
        const double current_value = *x;
        x += stride;
        if (ISNAN(current_value))
            return NA_INTEGER;
        if (!R_FINITE(current_value))
            continue;
        if (current_value > max_value) {
            number_of_ties = 1;
            max_value = current_value;
            max_index = i;
        } else if (current_value == max_value) {
            ++number_of_ties;
            if (number_of_ties * unif_rand() < 1.0)
                max_index = i;
        }
    }
    /* Adjust for R indexing (starts at 1) */
    return max_index + 1;
}

SEXP do_randomized_numeric_maxColumn(SEXP s_m) {
    SEXP s_ret;
    double *current_row, *current_value, max_value;
    int *ret;
    R_len_t row_index, column_index;
    
    UNPACK_REAL_MATRIX(s_m, m, nrow_m, ncol_m);
    s_ret = allocVector(INTSXP, nrow_m);
    PROTECT(s_ret);
    ret = INTEGER(s_ret);
    GetRNGstate();
    for (current_row = m, row_index = 0;
         row_index < nrow_m;
         ++row_index, ++current_row) {
        ret[row_index] = randomized_numeric_max(current_row, ncol_m, nrow_m);
    }
    PutRNGstate();
    UNPROTECT(1); /* s_ret */
    return s_ret;
}
