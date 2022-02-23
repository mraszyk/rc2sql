#ifndef __UTIL_H__
#define __UTIL_H__

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>

#define CHECK(x) assert(x);

int parseNumber(const char *s, size_t *pos, int *n);

#endif /* __UTIL_H__ */
