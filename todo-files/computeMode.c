#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP c_compute_mode(SEXP x) {
  const R_len_t n = length(x);
  double *xp = REAL(x);

  int curind = 0;
  int curcount = 1;
  int maxind = 0;
  int maxcount = 1;
  for (int i=1; i<n; i++) {
    if (xp[i] == xp[curind]) {
      curcount++;
    } else {
      /* Rprintf("i=%i, curind=%i, curcount=%i\n", i, curind, curcount); */
      if (curcount > maxcount) {
        maxcount = curcount;
        maxind = curind;
      }
      curcount = 1;
      curind = i;
    }
  }
  // handle last block
  if (curcount > maxcount) {
    maxcount = curcount;
    maxind = curind;
  }
  return ScalarInteger(maxind+1); // R is 1 based!
}


