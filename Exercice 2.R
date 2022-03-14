#                          Partie 1

# La moyenne
moyenne = function(vecteur) {
  sum =0
  for (val in vecteur) {
    sum = sum + val
  }
  return(sum/length(vecteur))
}

# La variance 
variance = function(vecteur) {
  SumCarre=0
  for (val in vecteur) {
    SumCarre = SumCarre + (val)^2
  }
  var = SumCarre/length(vecteur) - (moyenne(vecteur))^2
  return(var)
}

# l'ecart type
Ecartype = function(vecteur) {
  var =variance(vecteur)
  return(sqrt(var))
}

# La médiane
mediane = function(vecteur) {
  vecteur = order(vecteur, decreasing = TRUE)
  n=length(vecteur)
  if (n%%2 ==0) {
    med = (vecteur[n/2] + vecteur[1+n/2])/2
  }
  else {
    med = vecteur[(n+1)/2]
  }
  return(med)
}

# La covariance
covariance = function(X,Y) {
  n = length(X)
  SumProd = 0
  for (i in 1:n) {
    SumProd = SumProd + X[i] * Y[i]
  }
  cov = SumProd/n - moyenne(X)*moyenne(Y)
  return(cov)
  
}

#                                  Partie 2

# 1- Equation scond dégé
solve.2nd.degre = function(a,b,c) {
  d = b^2 -4*a*c
  if  (d < 0) {
    return(NA)
  }
  else if (d==0) {
    return(-b/(2*a))
  }
  else {
    x1 = (-b - sqrt(d))/(2*a)
    x2 = (-b + sqrt(d))/(2*a)
    return(c(x1,x2))
  }
  
}

# 2- factoriel d'un nombre
fact = function(n) {
  prod = 1
  for (i in 1:n) {
    prod = prod*i
  }
  return(prod)
}
