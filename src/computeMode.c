#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "macros.h"

SEXP c_compute_mode(SEXP s_x, SEXP s_ties_method) {
    UNPACK_INT_VECTOR(s_x, x, len_x);
    int max_index = 0;
    int i;
    int max_x = 0;
    int max_freq = 0;
    int max_freq_index = 0;
    int ties_method = asInteger(s_ties_method);

    /* get max int element of x. x is 1-based! */
    for (i = 0; i < len_x; i++) {
        /* Rprintf("i = %i: x = %i\n", i, x[i]); */
        if (x[i] > max_x)
            max_x = x[i];
    }

    /* alloc hashmap for all entries of x, then count their freqs in the hashmap */
    int *hashmap = calloc(max_x+1, sizeof(int));
    for (i = 0; i < len_x; i++)
        hashmap[x[i]]++;

    /* find the entry in the hashmap with max frequency */

    if (ties_method == 1) { /* "first": We search from the front */
        for (i = 1; i <= max_x; i++) {
            if (hashmap[i] > max_freq) {
                max_freq_index = i;
                max_freq = hashmap[i];
            }
        }
    } else if (ties_method == 2) { /* "end": We search from the end */
        for (i = max_x; i > 0; i--) {
            if (hashmap[i] > max_freq) {
                max_freq_index = i;
                max_freq = hashmap[i];
            }
        }
    }

  free (hashmap);
  return ScalarInteger(max_freq_index);
}

