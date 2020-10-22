# STAT-S 610
# LAB 3
# 2020-10-02
# https://jfukuyama.github.io/teaching/stat610/assignments/lab3.pdf

# --- functions --- #

llr <- function(x, y, z, omega) {
  fits <- sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

compute_f_hat <- function(z, x, y, omega) {
  w <- make_weight_matrix(z, x, omega)
  X <- make_predictor_matrix(x)
  f_hat <- c(1, z) %*% solve(t(X) %*% apply(X, 2, "*", rep(w))) %*% t(X) %*% w * y
  return(f_hat)
}


make_weight_matrix <- function(z, x, omega) {
  r <- abs(x - z) / omega  # this is a vector of the same length as x
  w <- sapply(r, W)  # this is a vector of the same length as x and r
  #Wz <- diag(w)  # this is a diagonal matrix with elements from w
  return(w)
}

W <- function(r) {
  if (abs(r) < 1) {
    return((1 - abs(r) ** 3) ** 3)
  } else {
    return(0)
  }
}

make_predictor_matrix <- function(x) {
  n <- length(x)
  return(cbind(rep(1, n), x))
}


# # --- example 1 --- #
# # 
# # # get the data
# data(french_fries, package = 'reshape2')
# french_fries <- na.omit(french_fries)
# 
# # input data
# x <- french_fries$potato
# y <- french_fries$buttery
# 
# # space along which to smooth
# z <- seq(0, 15, length.out = 100)
# omega=2
# 
# # run smoothing
# fits <- llr(z = z, x = x, y = y, omega = 2)
# 
# # plot the data and the smoother
# plot(x, y)
# lines(z, fits, col = 'red')

# 
# # --- example 2 --- #
# 
# # noisy sine wave
# x <- runif(1000, -2 * pi, 2 * pi)
# y <- sin(x) + rnorm(length(x))
# 
# # space along which to smooth
# z <- seq(-2 * pi, 2 * pi, length.out = 100)
# 
# # run smoothing
# fits <- llr(z = z, x = x, y = y, omega = pi / 3)
# 
# # plot the data and the smoother
# plot(x, y)
# lines(z, fits, col = 'red')
