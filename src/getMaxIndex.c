#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


#define UNPACK_REAL_VECTOR(S, D, N) \
  double *D = REAL(S); \
  const R_len_t N = length(S);         

/*
  Get index of maximal element of double array x (1-based).
  
  Allows to only consider a subset of elements, separated by regular gaps. 
  If NANs or NAs are encountered
  
  x       : double pointer to data
  n       : length of x
  stride  : step size to walk thru x
  
  return  : Index of maximal element (1-based) or 
            -1 if we did not find a maximal elemnt (empty vector or only removed NAs)
*/
int get_max_index(double *x, size_t n, size_t step, int ties_method, int na_rm) {
  size_t i;
  int max_index = -2;
  int number_of_ties = 0;
  double max_value = -DBL_MAX;
  
  for (i = 0; i < n; ++i) {
    const double current_value = x[i*step];
    if (!na_rm && ISNAN(current_value))
      return NA_INTEGER;
    if (current_value > max_value) {
      number_of_ties = 1;
      max_value = current_value;
      max_index = i;
    } else if (current_value == max_value) {
      if (ties_method == 1) {
        ++number_of_ties;
        if (number_of_ties * unif_rand() < 1.0)
          max_index = i;
      } else if (ties_method == 3) {
        max_index = i; 
      }
    }
  }
  /* make index 1-based */
  return max_index + 1;
}

SEXP c_getMaxIndex(SEXP s_x, SEXP s_ties_method, SEXP s_na_rm) {
  if (length(s_x) == 0)
    return NEW_INTEGER(0);
  int ties_method = asInteger(s_ties_method);
  int na_rm = asInteger(s_na_rm);
  UNPACK_REAL_VECTOR(s_x, x, len_x);
  GetRNGstate();
  int index = get_max_index(x, len_x, 1, ties_method, na_rm);
  PutRNGstate();
  if (index == -1)
    return NEW_INTEGER(0);
  else  
    return ScalarInteger(index);
}
