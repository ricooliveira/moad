N = rep(0,8)
R = rep(0,8)
UB = rep(1,8)
LB = rep(0,8)

delta = function(i){
  return( sqrt((3/2) * (log(i) / N[i])) )
}

