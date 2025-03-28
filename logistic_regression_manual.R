logis_reg <- function(x, y, m="n", t=1e-6, i=100) {
  x <- cbind(1, x)
  x <- as.matrix(x)
  r <- nrow(x)
  c <- ncol(x)
  p <- rep(0, c)

  for (j in 1:i) {
    n <- x %*% p
    q <- 1 / (1 + exp(-n))
    w <- diag(as.vector(q * (1 - q)))
    z <- n + (y - q) / (q * (1 - q))

    if (m == "n") {
      g <- t(x) %*% (y - q)
      h <- -t(x) %*% w %*% x
      pb <- p - solve(h) %*% g
    } else if (m == "i") {
      pb <- solve(t(x) %*% w %*% x) %*% t(x) %*% w %*% z
    } else {
      stop("gagal")
    }

    if (max(abs(pb - p)) < t) break
    p <- pb
  }

  return(list(m = m, p = p, q = q))
}

set.seed(123)
j <- 100
d <- data.frame(a = rnorm(j), b = rnorm(j))
d$c <- rbinom(j, 1, prob = 1 / (1 + exp(-(-1 + 2 * d$a - 3 * d$b))))

x <- as.matrix(d[, c("a", "b")])
y <- as.numeric(d$c)

k <- logis_reg(x, y, m="n")
print(k)

k <- logis_reg(x, y, m="i")
print(k)
