// Modified Damerau-Levenshtein distance.
//
// Compiled port of DamerauLevenshteinMod::mdld_php() from TNRSbatch, kept in
// step with the reference implementation tnrs_mdld_r() in R/local_mdld.R.  Any
// change here must keep those two in agreement; tests/testthat/test-local-mdld.R
// compares them over both real and randomly generated names.
//
// Comparison is on bytes, matching PHP's byte-based strlen() and indexing.

#include <Rcpp.h>
#include <algorithm>
#include <string>
#include <vector>

using namespace Rcpp;

static int mdld_one(const std::string &s1, const std::string &s2,
                    int block_limit, int max_distance) {
  if (s1 == s2) {
    return 0;
  }

  int len1 = static_cast<int>(s1.size());
  int len2 = static_cast<int>(s2.size());

  if (len1 == 0 || len2 == 0) {
    return std::max(len1, len2);
  }
  if (len1 == 1 && len2 == 1) {
    return 1;
  }

  // Trim common leading bytes, then common trailing bytes.  Indices are into
  // the original strings so that nothing is copied.
  int i = 0, j = 0;
  while (i < len1 && j < len2 && s1[i] == s2[j]) {
    ++i;
    ++j;
  }
  int e1 = len1 - 1, e2 = len2 - 1;
  while (e1 >= i && e2 >= j && s1[e1] == s2[e2]) {
    --e1;
    --e2;
  }

  int n1 = e1 - i + 1;
  int n2 = e2 - j + 1;
  if (n1 <= 0 || n2 <= 0) {
    return std::max(n1 < 0 ? 0 : n1, n2 < 0 ? 0 : n2);
  }
  if (n1 == 1 && n2 == 1) {
    // The strings differ, or the equality check above would have returned
    return 1;
  }

  // a[] and b[] are the trimmed strings, indexed from 1 to match the reference
  const char *a = s1.data() + i;
  const char *b = s2.data() + j;

  // Dense matrix, (n1 + 1) x (n2 + 1), row major
  const int ncol = n2 + 1;
  std::vector<int> m(static_cast<size_t>(n1 + 1) * ncol, 0);
  for (int t = 0; t <= n2; ++t) {
    m[t] = t;
  }
  for (int s = 0; s <= n1; ++s) {
    m[static_cast<size_t>(s) * ncol] = s;
  }

  const int base_block =
      std::min(std::min(n1 / 2, n2 / 2), block_limit);
  int current_distance = max_distance;

  for (int s = 1; s <= n1; ++s) {
    const size_t row = static_cast<size_t>(s) * ncol;
    const size_t prev = static_cast<size_t>(s - 1) * ncol;

    for (int t = 1; t <= n2; ++t) {
      const int this_cost = (a[s - 1] == b[t - 1]) ? 0 : 1;
      int block_length = base_block;

      if (block_length < 1) {
        m[row + t] = std::min(std::min(m[row + t - 1] + 1, m[prev + t] + 1),
                              m[prev + t - 1] + this_cost);
      }

      while (block_length >= 1) {
        bool transposed = false;

        if (s >= block_length * 2 && t >= block_length * 2) {
          // The two halves are swapped between the strings
          transposed =
              std::equal(a + (s - block_length * 2), a + (s - block_length),
                         b + (t - block_length)) &&
              std::equal(a + (s - block_length), a + s,
                         b + (t - block_length * 2));
        }

        if (transposed) {
          const size_t back =
              static_cast<size_t>(s - block_length * 2) * ncol +
              (t - block_length * 2);
          m[row + t] = std::min(
              std::min(m[row + t - 1] + 1, m[prev + t] + 1),
              m[back] + this_cost + (block_length - 1));
          block_length = 0;
        } else if (block_length == 1) {
          m[row + t] = std::min(std::min(m[row + t - 1] + 1, m[prev + t] + 1),
                                m[prev + t - 1] + this_cost);
        } else {
          m[row + t] = 0;
        }

        --block_length;
      }

      if (current_distance > m[row + t]) {
        current_distance = m[row + t];
      }
    }

    // Early abort, exactly as upstream: current_distance is the smallest cell
    // seen so far and is never reset, so this can only fire on the first column
    if (current_distance >= max_distance) {
      return current_distance;
    }
  }

  return m[static_cast<size_t>(n1) * ncol + n2];
}

// [[Rcpp::export]]
IntegerVector mdld_cpp(CharacterVector s1, CharacterVector s2, int block_limit,
                       int max_distance) {
  const R_xlen_t n = s1.size();
  IntegerVector out(n);

  for (R_xlen_t k = 0; k < n; ++k) {
    if (CharacterVector::is_na(s1[k]) || CharacterVector::is_na(s2[k])) {
      out[k] = NA_INTEGER;
      continue;
    }
    out[k] = mdld_one(as<std::string>(s1[k]), as<std::string>(s2[k]),
                      block_limit, max_distance);
  }

  return out;
}
