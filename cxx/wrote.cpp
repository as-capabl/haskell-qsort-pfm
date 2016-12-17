#include <wrote.h>

#include <algorithm>
#include <vector>


static longitr divide(unsigned long pivot, longitr start, longitr last);

extern "C" void quicksort_wrote(longitr begin, longitr end)
{
  std::ptrdiff_t len = end - begin;
  if (len <= 1) return;

  long pivot = *(begin + len / 2);
  longitr med = divide(pivot, begin, begin + (len - 1));

  quicksort_wrote(begin, med);
  quicksort_wrote(med, end);
}

static longitr divide(unsigned long pivot, longitr lower, longitr higher)
{
  while (lower <= higher) {
    while (lower <= higher && *lower < pivot) ++lower;
    while (lower <= higher && *higher > pivot) --higher;

    if (lower >= higher) break;

    std::swap(*lower, *higher);
    ++lower; --higher;
  }
  return lower;
}

extern "C" void quicksort_stl(longitr begin, longitr end)
{
  std::sort(begin, end);
}
