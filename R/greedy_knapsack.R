#' Greedy Knapsack Solver
#'
#' Solves the Knapsack problem using a greedy heuristic. The function selects items
#' based on their value-to-weight ratio (vi/wi) and adds them to the knapsack as long as they fit.
#' This method does not guarantee the optimal solution but provides a quick approximation.
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

greedy_knapsack <- function(x, W){
  stopifnot(is.data.frame(x),
           all(c("v", "w") %in% names(x)),
           is.numeric(x$v), is.numeric(x$w),
           all(is.finite(x$v)), all(is.finite(x$w)),
           all(x$v > 0), all(x$w > 0),
           length(W) == 1, is.numeric(W), is.finite(W), W > 0
  )

v <- x$v
w <- x$w

n <- length(v)


ratio <- v / w

items <- order(ratio, decreasing = TRUE)

total_value <- 0
total_weight <- 0
selected_items <- integer(0)

for (i in items) {

  if (total_weight + w[i] <= W) {
    total_value <- total_value + v[i]
    total_weight <- total_weight + w[i]
    selected_items <- c(selected_items, i)
  }
}


list(value = total_value, elements = selected_items)

}
