getSmoothR  <-  function( t, s, Z, F, KG )

{ 

 R <-  array(0,dim =c(q,p,s))

 R[,,t] <- Z[ , ,s] *% F[ , ,t]%*%(diag(q) - M[ , ,t]%*%Z[ , ,t-1]




for( k  in  t:s-1 )
R[,,k+1] <- R[,,k] %*% F[ , ,k]%*%(diag(p) - M[ , ,k]%*%Z[ , ,k-1] 

return(R)

}

