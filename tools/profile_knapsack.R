devtools::load_all()
library(profvis)
library(bench)

set.seed(42)
n <- 2000
x <- data.frame(
  w = sample(1:4000, n, TRUE),
  v = runif(n, 0, 10000)
)
W <- 3500


# 1) Brute force
pv_brute <- profvis::profvis({
  brute_force_knapsack(x[1:20, ], W)
}, interval = 0.005)

# 2) Parallel brute force
pv_parallel <- profvis::profvis({
  parallel_brute_force_knapsack(x[1:20, ], W)
}, interval = 0.005)

# 3) Dynamic programming
pv_dp <- profvis::profvis({
  knapsack_dynamic(x[1:900, ], W)
}, interval = 0.005)

# 4) Greedy:
Ws  <- sample(1500:4000, 2000, TRUE)
pv_greedy <- profvis::profvis({
  for (k in seq_len(2000)) {
    xk <- x[sample.int(n, n), , drop = FALSE]   # reshuffle rows
    greedy_knapsack(xk, Ws[k])
  }
}, interval = 0.005)

pv_brute; pv_parallel; pv_dp; pv_greedy


bench::mark(
  brute    = brute_force_knapsack(x[1:20, ],  W),
  parallel = parallel_brute_force_knapsack(x[1:20, ], W),
  dynamic  = knapsack_dynamic(x[1:900, ],     W),
  greedy   = greedy_knapsack(x[1:2000, ],     W),
  iterations = 50,
  check = FALSE,
  memory = FALSE
)


utils::Rprofmem("mem.out")
invisible(knapsack_dynamic(x[1:900, ], W))
utils::Rprofmem(NULL)

cat("\nDone. Open pv_* objects to view flame graphs. See bench table in console.\n")
