




logistic <- function(t, r, K, N0) {
  + K * N0 * exp(r * t) / (K + N0 * (exp(r * t) - 1))
  + }
plot(0:100, logistic(t = 0:100, r = 0.1, K = 10, N0 = 0.1))