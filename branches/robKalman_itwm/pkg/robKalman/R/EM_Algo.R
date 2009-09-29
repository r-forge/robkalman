EMAlgoSSM <- function(y, a, Z, initStepEM = initStepSSMclass,
                      EStepEM = EStepSSMclass,
		                  MStepEM = MStepSSMclass,
                      eabs, erel, maxit,
                      verbose = FALSE, ...)
{

##main/frame-function for the EM Algorithm(à la Shumway&Stoffer,1981)

    eabs <- rep(eabs, length.out = 3)
    erel <- rep(erel, length.out = 3)
    if(is.null(names(eabs))) names(eabs) <- c("F","Q","V")
    if(is.null(names(erel))) names(erel) <- c("F","Q","V")

    qd <- if(length(dim(Z)) 1 else (dim(Y))[1]
    pd <- if(length(dim(Z)) 1 else (dim(Z))[2]
    tt <- (dim(Y))[3]
    runs <- (dim(Y))[2]


    Fa <- array(0,dim=c(pd,pd,runs))
    Qa <- array(0,dim=c(pd,pd,runs))
    Va <- array(0,dim=c(qd,qd,runs))
    ka <- numeric(runs)
    
    for(i in 1:runs){
    resinitStepEM <- initStepEM(y,Z)

    F <- resinitStepEM$F
	  Q <- resinitStepEM$Q
	  V <- resinitStepEM$V


   log.F <- TRUE
   log.Q <- TRUE
   log.V <- TRUE
   k <- 0

   while((k<maxit) || log.F || log.V || log.Q )
	 {


##NOTE: In order to be able to run the EM Algorithm recursively, we need to compute
##those values first, so we just need a starting value for the differences of the 
##norms for the parameters V,F and Q. Hence we just run the E and M-steps once.
##For the E-Step, one also needs input parameters a and S, so:
	  ##a<-miu_init;
	  ##S<-sigma_init;

	    resEStepEM <- EStepEM(y, a, S=Q, F, Q, Z, V, dropRuns = FALSE, ...)

      F_old <- F
	    Q_old <- Q
	    V_old <- V

## define now the parameters needed for the M-Step:
      x <- resEStepEM$x
      P <- resEStepEM$P
      Pdep <- resEStepEM$Pdep

##Call function MStepEM()...

  	  resMStepEM <- MStepEM(M = Z, y = y, # matrix(y[,1,],qd,T),
	                        x = x, P = P, Pdep = Pdep, ...)

	
	    F <- resMStepEM$F
	    Q <- resMStepEM$Q
	    V <- resMStepEM$V

      log.F <- .isDeltaSmall(F_old,F,e.rel["F"],e.abs["F"])
      log.Q <- .isDeltaSmall(Q_old,Q,e.rel["Q"],e.abs["Q"])
      log.V <- .isDeltaSmall(V_old,V,e.rel["V"],e.abs["V"])


   		k <- k+1
		  if(verbose)
       print(list(k=k,x=x,F=F,Q=Q,V=V));
    }
            
    Fa[,,i] <- F
    Qa[,,i] <- Q
    Va[,,i] <- V
    ka[i] <- k
 }
#############END of Algorithm###################
##OUTPUT:
list(F=Fa, Q=Qa, V=Va, Z=Z, k=ka)

}

##Call this function(see how it works)
##EmAlgo.intern(Z,y,Q_init=Q,F_init=F,V_init=V,EStepEM=recursiveFilter,MStepEM=MStep,eabs,erel,maxit,noise=NULL)

.isDeltaSmall <- function(x.old, x.new, e.rel, e.abs){
 no.a <- (sum((x.old-x.new)^2))^.5
 no.r <- no.a / (sum((x.old)^2))^.5
 return(no.a < e.abs && no.r < e.rel)
 }
