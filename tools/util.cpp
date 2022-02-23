#include "util.h"

#include <climits>
#include <cstdlib>

int parseNumber(const char *s, size_t *pos, int *n)
{
    int i = (pos == NULL ? 0 : *pos);
    int ans = 0;
    if (!('0' <= s[i] && s[i] <= '9')) return 1;
    while ('0' <= s[i] && s[i] <= '9') {
        int d = s[i++] - '0';
        if (ans > INT_MAX / 10) return 1;
        ans *= 10;
        if (ans >= INT_MAX - d) return 1;
        ans += d;
    }
    if (pos != NULL) *pos = i;
    *n = ans;
    return 0;
}
