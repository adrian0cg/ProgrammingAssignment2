test.hilbert <- function() {
  # given
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  h8 <- hilbert(8)
  sh8 <- solve(h8)
  # when
  h8pa <- makeCacheMatrix()
  h8pa$set(h8)
  sh8pa <- cacheSolve(h8pa)
  res <- round(sh8pa %*% h8, 5)
  diagonals <- diag(res)
  offDiagonals <- (diag(res) <- 0)
  # then
  checkTrue(all(diagonals == 1))
  checkTrue(all(offDiagonals == 0))
}
