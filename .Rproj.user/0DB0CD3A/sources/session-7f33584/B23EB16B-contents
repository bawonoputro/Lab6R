#' Dynamic Programming Knapsack
#'
#' Solves the Knapsack problem using dynamic programming. The function
#' calculates the maximum value that can be obtained by selecting a subset of items
#' such that their total weight does not exceed the given knapsack capacity.
#'
#' This approach uses a **bottom-up** DP algorithm and iterates over the items and
#' capacities, building the solution table row by row. The function then uses **backtracking**
#' to recover the list of items selected to achieve the maximum value.
#'
#' @param x A data frame with two columns:
#'   - `v`: A numeric vector representing the values of the items.
#'   - `w`: A numeric vector representing the weights of the items.
#'   Both vectors must be positive, numeric, and finite.
#' @param W A single positive numeric value representing the knapsack's capacity.
#' @return A list containing:
#'   - `value`: Maximum total value achievable.
#'   - `elements`: A vector of the selected items that give the maximum value.
#' @export

knapsack_dynamic <- function(x, W){
  stofinot(is.data.frame(x),
           all(c("v", "w") %in% names(x)),
           is.numeric(x$v), is.numeric(x$w),
           all(is.finite(x$v)), all(is.finite(x$w)),
           all(x$v > 0), all(x$w > 0),
           length(W) == 1, is.numeric(W), is.finite(W), W > 0
           )
  n <- row(x)
  if (n == 0L) return(list(value = 0, elements = integer()))

  v <- as.numeric(x$v)
  w <- as.integer(round(x$w))
  W <- as.integer(round(W))

  best_value <- 0
  best_idx   <- integer()

  m <- matrix(0, nrow = n + 1, ncol = W + 1)

  for (i in 1:n) {
    wi <- w[i]; vi <- v[i]
    for (j in 1:W) {
      if (wi > j) {
        m[i + 1, j + 1] <- m[i, j + 1]
      } else {
        m[i + 1, j + 1] <- max(m[i, j + 1],
                                 m[i, (j - wi) + 1] + vi)
      }
    }
  }

  elements <- integer(0)
  j <- W
  for (i in n:1) {
    if (m[i + 1, j + 1] != m[i, j + 1]) {
      elements <- c(elements, i)
      j <- j - w[i]
      if (j <= 0) break
    }
  }

  list(value = as.numeric(m[n + 1, W + 1]),
       elements = sort(as.integer(elements)))


}
