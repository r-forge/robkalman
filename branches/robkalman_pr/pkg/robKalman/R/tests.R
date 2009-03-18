mySSM=TI.SSM(F=matrix(c(2,-1,0.2,1),2,2),Z=t(c(2,1)),Q=matrix(c(2,1,1,1),2,2),V=2, Tn=10)
simulate(mySSM)[c("states","obs")]
simulate(mySSM, nsim =2)[c("states","obs")]
simulate(mySSM, drop = TRUE)[c("states","obs")]
simulate(mySSM, nsim =2, drop = TRUE)[c("states","obs")]
mySSM=TI.SSM(F=0.8,Z=1,Q=1,V=2, Tn=10)
simulate(mySSM)[c("states","obs")]
simulate(mySSM, nsim =2)[c("states","obs")]
simulate(mySSM, drop = TRUE)[c("states","obs")]
simulate(mySSM, nsim =2, drop = TRUE)[c("states","obs")]
mySSM=TI.SSM(F=matrix(c(.2,-1,0.2,1),2,2),Z=F,Q=matrix(c(2,1,1,1),2,2),V=Q, Tn=10)
simulate(mySSM)[c("states","obs")]
simulate(mySSM, nsim =2)[c("states","obs")]
simulate(mySSM, drop = TRUE)[c("states","obs")]
simulate(mySSM, nsim =2, drop = TRUE)[c("states","obs")]
mySSM=TI.SSM(F=.8,Z=c(2,1),Q=1,V=diag(2), Tn=10)
simulate(mySSM)[c("states","obs")]
simulate(mySSM, nsim =2)[c("states","obs")]
simulate(mySSM, drop = TRUE)[c("states","obs")]
simulate(mySSM, nsim =2, drop = TRUE)[c("states","obs")]
