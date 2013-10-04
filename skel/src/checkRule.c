#include <stdio.h>              /* snprintf */
#include <ctype.h>              /* isdigit  */
#include <string.h>             /* strlen   */
#include "any.missing.h"
#include "checkRule.h"

static char errmsg[MSGLEN];

static SEXP typeError(SEXP x, char *typename) {
    snprintf(errmsg, MSGLEN, "'%%s' must be of type '%s', not '%s'", typename, type2char(TYPEOF(x)));
    return ScalarString(mkChar(errmsg));
}

SEXP c_checkRule(SEXP x, SEXP rule) {
    if (!isString(rule) || length(rule) != 1)
        error("Argument 'rule' must be a scalar string");

    Rboolean check_missing = FALSE;
    R_len_t ne, nx;
    const char *str = NULL;
    str = CHAR(STRING_ELT(rule, 0));
    const R_len_t nchars = strlen(str);

    if (nchars == 0)
        error("Argument 'rule' must be a string with at least one character");

    /* check class
       TODO: 'D' for data.frame without missing values; same for lists of atomic types
       -> use an argument here
    */
    switch(str[0]) {
        case 'B': check_missing = TRUE;
        case 'b': if (!isLogical(x)) return typeError(x, "logical"); break;
        case 'I': check_missing = TRUE;
        case 'i': if (!isInteger(x)) return typeError(x, "integer"); break;
        case 'R': check_missing = TRUE;
        case 'r': if (!isReal(x)) return typeError(x, "double"); break;
        case 'N': check_missing = TRUE;
        case 'n': if (!isInteger(x) && !isReal(x)) return typeError(x, "numeric"); break;
        case 'C': check_missing = TRUE;
        case 'c': if (!isComplex(x)) return typeError(x, "complex"); break;
        case 'S': check_missing = TRUE;
        case 's': if (!isString(x)) return typeError(x, "character"); break;
        case 'L': check_missing = TRUE;
        case 'l': if (!isNewList(x) || isFrame(x)) return typeError(x, "list"); break;
        case 'A': check_missing = TRUE;
        case 'a': if (!isVectorAtomic(x)) return typeError(x, "atomic"); break;
        case 'M': check_missing = TRUE;
        case 'm': if (!isMatrix(x)) return typeError(x, "matrix"); break;
        case 'f': if (!isFunction(x)) return typeError(x, "function"); break;
        case 'e': if (!isEnvironment(x)) return typeError(x, "environment"); break;
        case 'd': if (!isFrame(x)) return typeError(x, "data.frame"); break;
        case '0': if (!isNull(x)) return typeError(x, "NULL"); break;
        case '*': break;
        default: error("Unknown type identifier '%c'", str[0]);
    }

    if (check_missing && any_missing(x)) {
        snprintf(errmsg, MSGLEN, "'%%s' may not contain missing values");
        return ScalarString(mkChar(errmsg));
    }

    if (nchars >= 2) {
        nx = length(x);
        /* TODO: dimensions for arrays/matrix: [d+],[d+] */
        switch(str[1]) {
            case '*': break;
            case '?':
                if(nx > 1) {
                   snprintf(errmsg, MSGLEN, "'%%s' must have length <= 1");
                   return ScalarString(mkChar(errmsg));
                }
                break;
            case '+':
                if (nx == 0) {
                    snprintf(errmsg, MSGLEN, "'%%s' must have length > 0");
                    return ScalarString(mkChar(errmsg));
                }
                break;
            default:
                for (R_len_t i = 1; i < nchars; i++) {
                    if (!isdigit(str[i]))
                        error("Invalid length definition. Expected number, got '%c'", str[i]);
                }
                ne = atoi(str+1);
                if (nx != ne) {
                    snprintf(errmsg, MSGLEN, "'%%s' must have length %i, got %i", ne, nx);
                    return ScalarString(mkChar(errmsg));
                }
        }
    }

    return ScalarLogical(TRUE);
}
