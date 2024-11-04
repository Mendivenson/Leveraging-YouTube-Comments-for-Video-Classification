locToVec <-
function(x, n = 10, symmetrical = FALSE){
  if (symmetrical){
    vec = rep(0, (n * (n - 1))/2)
  } else {
    vec = rep(0, n^2)
  }
  vec[x] = 1
  return(vec)
}
