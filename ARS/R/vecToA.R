vecToA <-
function(x, n = 10, symmetrical = FALSE){
  if (symmetrical){
    A = matrix(0, ncol = n, nrow = n)
    A[lower.tri(A)] = x
    A = A + t(A)
  } else {
    A = matrix(x, ncol = n, nrow = n)
  }
  return(A)
}
