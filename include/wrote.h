#ifndef WROTE_H
#define WROTE_H

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned long* longitr;

void quicksort_wrote(longitr begin, longitr end);

void quicksort_stl(longitr begin, longitr end);

#ifdef __cplusplus
}
#endif

#endif //WROTE_H
