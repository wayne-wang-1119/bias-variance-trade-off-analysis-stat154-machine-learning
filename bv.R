Dtest = runif(10, min = -1, max = 1) ##Q1
Ytest = sin(pi * Dtest)+ rnorm(length(Dtest), 0 , 1/2)
for(p in seq(1, 10)){
  p = p
  X = list()
  flist=list()
  DS = list()
  YS = list()
  betalist = list()
  bia = list()
  var = list()
  
  ##generate S from 1 to 500, each of size 50, with corresponding y
  for(a in 1:500){
    S = runif(50, -1, 1)
    y = sin(pi * S) + rnorm(50, 0, 1/2)
    DS = append(DS, list(S))
    YS = append(YS, list(y))
  }
  
  ##create feature matrix for each S, find beta
  for(b in 1:500){
    xs = as.matrix(rep(1, 50))
    x = DS[[b]]
    y = YS[[b]]
    for(c in 1:p){
      xj=as.matrix(x^c)
      xs = cbind(xs,xj) ##create X feature
    }
    beta = (solve(t(xs)%*%xs)) %*% t(xs) %*% y ##closed form as given
    betalist = append(betalist, list(beta))
  }
  
  ##loop through each of the Xtest xs and record predicted f and real y
  e1 = list()
  e2 = list()
  sbias = list()
  svars = list()
  for(i in 1:length(Dtest)){
    xi = Dtest[i]
    yi = Ytest[i]
    x_test = 1
    for(j in 1:p){
      x_test = append(x_test, xi^j) ##creating x_test for ith x
    }
    fj = list()
    for (n in 1:500){
      fj = append(fj, betalist[[n]] %*% x_test)
    }
    fbarj = sum(as.numeric(fj))/(500) ##f_bar for xj
    
    e1 = (yi - fbarj)^2
    for(m in 1:500){
      e2 = append(e2, (as.numeric(fj[m]) - fbarj)^2)
    }
    sbias = append(sbias, e1)
    svars = append(svars, (1/500)*sum(as.numeric(e2)))
  }
  bia = append(bia, sum(as.numeric(sbias))/(length(Dtest)))
  var = append(var, sum(as.numeric(svars))/(length(Dtest)))
}