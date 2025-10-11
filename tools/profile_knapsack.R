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

# --- A) profvis flame graphs -------------------------------------------------
# Use >= 5 ms sampling to avoid “interval too short” warnings.

# 1) Brute force (keep N small; exponential)
pv_brute <- profvis::profvis({
  brute_force_knapsack(x[1:20, ], W)
}, interval = 0.005)

# 2) Parallel brute force (same N; may fall back to sequential depending on OS)
pv_parallel <- profvis::profvis({
  parallel_brute_force_knapsack(x[1:20, ], W)
}, interval = 0.005)

# 3) Dynamic programming (O(n*W)): bump n enough to get some samples
pv_dp <- profvis::profvis({
  knapsack_dynamic(x[1:900, ], W)
}, interval = 0.005)

# 4) Greedy: very fast → make workload heavier by running many varied instances
#    This creates many capacities and reshuffles rows so each run does real work.
Ws  <- sample(1500:4000, 2000, TRUE)   # capacities to try
pv_greedy <- profvis::profvis({
  for (k in seq_len(2000)) {
    xk <- x[sample.int(n, n), , drop = FALSE]   # reshuffle rows
    greedy_knapsack(xk, Ws[k])
  }
}, interval = 0.005)

# Open these in RStudio to inspect flame graphs:
pv_brute; pv_parallel; pv_dp; pv_greedy

# --- B) Benchmarks (separate from profvis) -----------------------------------
# Compare typical sizes for each algorithm family.
bench::mark(
  brute    = brute_force_knapsack(x[1:20, ],  W),
  parallel = parallel_brute_force_knapsack(x[1:20, ], W),
  dynamic  = knapsack_dynamic(x[1:900, ],     W),
  greedy   = greedy_knapsack(x[1:2000, ],     W),
  iterations = 50,
  check = FALSE,
  memory = FALSE
)

# --- C) Optional: memory profiling for DP ------------------------------------
# Produces 'mem.out' you can inspect for large allocations.
utils::Rprofmem("mem.out")
invisible(knapsack_dynamic(x[1:900, ], W))
utils::Rprofmem(NULL)

cat("\nDone. Open pv_* objects to view flame graphs. See bench table in console.\n")
