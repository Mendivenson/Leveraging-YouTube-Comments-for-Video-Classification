vec <-
function(A, symmetrical = TRUE){
  n = ncol(A)
  vec = c()
  if (symmetrical){
    A[upper.tri(A, diag = T)] = NA
  }
  for (i in 1:ncol(A)){
    vec = c(vec, A[!is.na(A[,i]),i])
  }
  return(vec)
}
