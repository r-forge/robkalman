require(robKalman)


#######################################
#Preparation
#######################################


makeDF<-function(X,erg1,erg2, nam1="rLS(eff)", nam2="rLS(r)", withY=FALSE, withInd=TRUE,ncoord,Y=NULL)
{
DDa<-list(NULL)
if(ncoord==1)
  {DD <- as.data.frame(cbind("act. state"=t(X), "classK."=t(erg1$Xf), 
                        t(erg1$Xrf), t(erg2$Xrf)))
     names(DD)[3:4] <- c(nam1,nam2)                        
     if (withY) {yy<-c(NA,t(Y[1,])); DD <- as.data.frame(cbind(DD,"y"=yy))}
     if (withInd) 
         {
          xclip<-c(FALSE,as.logical(erg1$IndAO))
          DD <- as.data.frame(cbind(DD,"Ind"=xclip))
         }
     DDa<-list(DD)
  }
else
for(coord in 1:ncoord)
   { DD <- as.data.frame(cbind("act. state"=X[coord,], "classK."=erg1$Xf[coord,], 
                        erg1$Xrf[coord,], erg2$Xrf[coord,]))
     names(DD)[3:4] <- c(nam1,nam2)                        
     if (withInd) 
         {
          xclip<-c(FALSE,as.logical(erg1$IndAO))
          DD <- as.data.frame(cbind(DD,"Ind"=xclip))
         }
     DDa[[coord]]<-DD
    }
DDa
}

myplot<-function(DF, TT, coord)
{
matplot(0:TT,DF[,names(DF)!="Ind"], col=c("black","red","blue", "green", "gray"),
            type="l", lwd=2, lty=1, 
            ylab=paste(coord, ". coordinate of state",sep=""), 
            xlab="time")

if(any("Ind"==names(DF)))
    {xclip <- (0:TT)[as.logical(DF[,"Ind"])]
     points(xclip,0*xclip,pch="x")      
     }
}

plot44<-function(DFlist.id, DFlist.re, pd, TT)
{par(mfcol=c(pd,2),lwd=2,lty=1,pty = "m",mar=c(3.1,4.1,2.1,4.1))
 fct<-function(i,DFL){myplot(DFL[[i]], TT=TT, i)}
 sapply(1:pd, fct, DFlist.id)  
 sapply(1:pd, fct, DFlist.re)  
 par(mfcol=c(1,1))
}


simKalmanIdRe <-function(tt = TT, a = a0, Ss = SS0, F = F0, Q = Q0,  Z = Z0, Vi = V0i, 
                         mc = m0c, Vc = V0c, r = ract, rcalib=r1, effcalib=eff1) 
{
#Simulation::
X  <- simulateState(a = a0, S = Ss, F = F, Q = Q, tt = tt, runs = 1)
Yid  <- simulateObs(X = X, Z = Z, Vi = Vi, mc = mc, Vc = Vc, r = 0, runs = 1)
Yre  <- simulateObs(X = X, Z = Z, Vi = Vi, mc = mc, Vc = Vc, r = ract, runs = 1)

pd <- dim(X)[1]
qd <- dim(Yid)[1]

SS <- limitS(S = Ss, F = F, Q = Q, Z = Z, V = Vi)
### calibration b
# by efficiency in the ideal model
# efficiency  =  0.9
(B1 <- rLScalibrateB(eff = eff1, S = SS, Z = Z, V = Vi))
# by contamination radius
# r  =  0.1
(B2 <- rLScalibrateB(r = r1, S = SS, Z = Z, V = Vi))
### evaluation of rLS
rerg1.id <- rLSFilter(Yid, a = a, S = Ss, F = F, Q = Q, Z = Z, V = Vi, b = B1$b)
rerg1.re <- rLSFilter(Yre, a = a, S = Ss, F = F, Q = Q, Z = Z, V = Vi, b = B1$b)
rerg2.id <- rLSFilter(Yid, a = a, S = Ss, F = F, Q = Q, Z = Z, V = Vi, b = B2$b)
rerg2.re <- rLSFilter(Yre, a = a, S = Ss, F = F, Q = Q, Z = Z, V = Vi, b = B2$b)


DF.id <- makeDF(X,rerg1.id, rerg2.id, withY=(pd==qd&pd==1), ncoord=pd, Y=Yid)
DF.re <- makeDF(X,rerg1.re, rerg2.re, withY=(pd==qd&pd==1), ncoord=pd, Y=Yre)
#print(DF.id)

plot44(DF.id, DF.re, pd, tt)

###evaluation of MSE

###ideal situation
MSEid <- c("class.Kalman"=mean((X - rerg1.id$Xf)^2), ### MSE averaged over time
           "rLS.eff"=mean((X - rerg1.id$Xrf)^2),
           "rLS.r"=mean((X - rerg2.id$Xrf)^2))
         
###real situation
MSEre <- c("class.Kalman"=mean((X - rerg1.re$Xf)^2), ### MSE averaged over time
           "rLS.eff"=mean((X - rerg1.re$Xrf)^2),
           "rLS.r"=mean((X - rerg2.re$Xrf)^2))

print(list("Ideal situation"=MSEid,"Real situation"=MSEre))

op <- par(ask = interactive())
}


##############################################################
###Example1
##############################################################

#Hyper-Parameter

## p=2, q=1
a0   <- c(1, 0)
SS0  <- matrix(0, 2, 2)
F0   <- matrix(c(.7, 0.5, 0.2, 0), 2, 2)
Q0   <- matrix(c(2, 0.5, 0.5, 1), 2, 2)
TT   <- 50

Z0   <- matrix(c(1, -0.5), 1, 2)
V0i  <- 1
m0c  <- -20
V0c  <- 0.1
ract <- 0.1

r1<-0.1
eff1<-0.9

set.seed(361)
simKalmanIdRe()



###Example2
#Hyper-Parameter  one-dimensional steady state model
a0   <- 1
SS0  <- 0.2
F0   <- 1
Q0   <- 1
TT   <- 40

Z0   <- 1
V0i  <- 1
m0c  <- -30
V0c  <- 0.1
ract <- 0.1

r1<-0.3
eff1<-0.8

set.seed(361)
simKalmanIdRe()


###Example3
#Hyper-Parameter
## p=2, q=3

a0   <- c(1, 0)
SS0  <- matrix(0, 2, 2)
F0   <- matrix(c(.7, 0.5, 0.2, 0), 2, 2)
Q0   <- matrix(c(3, 0.5, 0.5, 1), 2, 2)
TT   <- 40

Z0   <- matrix(c(1, -0.5,2,0,1,1), 3, 2)
V0i  <- diag(c(1,2,0.2))
m0c  <- -40*c(1,-1,0)
V0c  <- diag(c(0.1,0.2,12))
ract <- 0.06

r1<-0.1
eff1<-0.4

set.seed(361)
simKalmanIdRe()
