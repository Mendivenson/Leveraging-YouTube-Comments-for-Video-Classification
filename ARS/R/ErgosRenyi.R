ErgosRenyi <-
function(n = 10, symmetrical = TRUE, prob = 0.5){
  A = matrix(0, ncol = n, nrow = n)
  edges = rbinom(n = (n*(n-1))/2, size = 1, prob = prob)
  A[upper.tri(A)] = edges
  if (symmetrical){
    A = A + t(A)
  } else {
    edges = rbinom(n = (n*(n-1))/2, size = 1, prob = prob)
    A[lower.tri(A)] = edges
  }
  return(A)
}
