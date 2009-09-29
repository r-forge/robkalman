MakePositive <- function(matrix){
### forces negative eigen values
### of a symmetric matrix to 0
if(length(matrix)==1) return ((matrix>0)*matrix)
X.eigen <- eigen(matrix, symmetric=TRUE)
V <- X.eigen$vectors
D <- X.eigen$values
D[X.eigen$values<0] <- 0
return(V%*%diag(D)%*%t(V))
}