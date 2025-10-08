#' Brute-force Knapsack
#'
#' Solves the Knapsack problem using a brute-force approach by evaluating all possible combinations of items.
#'
#' @param x A data frame with two columns:
#'   - `v`: A numeric vector representing the values of the items.
#'   - `w`: A numeric vector representing the weights of the items.
#'   Both vectors must have the same length, and all values should be positive and finite.
#' @param W A single positive numeric value representing the knapsack's capacity.
#' @return A list containing:
#'   - `value`: Maximum total value achievable.
#'   - `elements`: A vector of the selected items that give the maximum value.
#' @export


brute_force_knapsack <- function(x,W){
  stopifnot(is.data.frame(x),
            all(c("v","w") %in% names(x)),
            is.numeric(x$v), is.numeric(x$w),
            all(is.finite(x$v)), all(is.finite(x$w)),
            all(x$v > 0), all(x$w > 0),
            length(W) == 1, is.numeric(W), is.finite(W), W > 0
            )
  n <- nrow(x)

  if (length(n) != 1) {
    stop("n should be a single number.")
  }

  if (n == 0L) return(list(value = 0, elements = integer()))

  v <- as.numeric(x$v)
  w <- as.numeric(x$w)

  best_value <- 0
  best_idx   <- integer()

  last <- as.integer(2^n - 1L)
  for (i in 0:last) {
    bits <- intToBits(i)[seq_len(n)]
    selected  <- which(as.integer(bits) == 1L)

    total_w <- if (length(selected)) sum(w[selected]) else 0
    if (total_w <= W) {
      total_value <- if (length(selected)) sum(v[selected]) else 0
      if (total_value > best_value) {
        best_value <- total_value
        best_idx   <- selected
      }
    }
  }

  list(value = as.numeric(best_value), elements = as.integer(best_idx))
}
