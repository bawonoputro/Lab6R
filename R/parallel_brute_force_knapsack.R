#' Parallel Brute-force Knapsack
#'
#' Solves the Knapsack problem using a parallel brute-force approach by evaluating all
#' possible combinations of items using more than one core simultaneously.
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
parallel_brute_force_knapsack <- function(x,W){
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

  if (n == 0) return(list(value = 0, elements = integer()))

  v <- as.numeric(x$v)
  w <- as.numeric(x$w)

  best_value <- 0
  best_idx   <- integer()

  last <- as.integer(2^n - 1)

  num_cores <- parallel::detectCores() - 1
  chunk_size <- floor((last + 1) / num_cores)

  subset_range_list <- split(0:last, ceiling(seq_along(0:last) / chunk_size))

  evaluate_subset_chunk <- function(subset_range) {
    local_best_value <- 0
    local_best_idx <- integer()

  for (i in subset_range) {
    bits <- intToBits(i)[seq_len(n)]
    selected  <- which(as.integer(bits) == 1)

    total_w <- if (length(selected)) sum(w[selected]) else 0
    if (total_w <= W) {
      total_value <- if (length(selected)) sum(v[selected]) else 0
      if (total_value > local_best_value) {
        local_best_value <- total_value
        local_best_idx   <- selected
      }
    }
  }

  list(value = as.numeric(local_best_value), elements = as.integer(local_best_idx))

  }
  result_list <- parallel::mclapply(subset_range_list, evaluate_subset_chunk, mc.cores = num_cores)

  for (result in result_list) {
    if (result$value > best_value) {
      best_value <- result$value
      best_idx <- result$elements
    }
  }

  list(value = as.numeric(best_value), elements = as.integer(best_idx))
}

