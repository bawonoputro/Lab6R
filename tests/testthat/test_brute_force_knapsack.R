# Suppress warnings and set the RNG seed for reproducibility
suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")

# Test Data
n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)


# Test that the correct object is returned
test_that("Correct object is returned", {
  expect_silent(bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(bfk, c("value", "elements"))
})

# Test that the function rejects erroneous input
test_that("functions rejects erroneous input.", {
  expect_error(brute_force_knapsack("hej", 3500))
  expect_error(brute_force_knapsack(x = knapsack_objects[1:8,], W = -3500))
})

# Test for correct results for different inputs
test_that("Function returns correct results.", {
  bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(bfk$value, 16770, tolerance = 1e-3)
  expect_setequal(bfk$elements, c(5, 8))   # order-agnostic

  bfk <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(bfk$value, 16770, tolerance = 1e-3)
  expect_setequal(bfk$elements, c(5, 8))

  bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(bfk$value, 15428, tolerance = 1e-3)
  expect_setequal(bfk$elements, c(3, 8))

  bfk <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(bfk$value, 15428, tolerance = 1e-3)
  expect_setequal(bfk$elements, c(3, 8))
})


# Test for execution time of normal brute force
test_that("Normal brute force execution time", {
  st <- system.time(bfk <- brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[3] >= 0.00)  # Expect the elapsed time to be positive
})

# Test for parallel brute force execution time and result consistency
test_that("Parallel brute force execution time and result consistency", {
  # Measure time for normal brute force
  normal_time <- system.time({
    normal_result <- brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000)
  })

  # Measure time for parallel brute force
  parallel_time <- system.time({
    parallel_result <- parallel_brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000)
  })

  print(paste("Normal brute force execution time: ", normal_time["elapsed"], " seconds"))
  print(paste("Parallel brute force execution time: ", parallel_time["elapsed"], " seconds"))

  # Check if results are the same
  expect_equal(normal_result$value, parallel_result$value)
  expect_equal(normal_result$elements, parallel_result$elements)

  # Check if parallel version is faster (optional, you can set a threshold)
  expect_true(parallel_time["elapsed"] < normal_time["elapsed"],
              info = paste("Parallel execution time:", parallel_time["elapsed"],
                           "Normal execution time:", normal_time["elapsed"]))
})
