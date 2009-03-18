SSMellDistribution.f <- function(SSM,
         r.init = mvrnorm, r.innov = mvrnorm, r.obs = mvrnorm){
         SSM <- makeArrayRepresentation(SSM)
         Tn <- length(SSM@time)

         S.init <- matrix(.stdRepres(SSM@S, SSM@p, SSM@p, withTest = TRUE,
                               Tn = 2, time = time(SSM@time)[1]),
                     nrow = SSM@p, ncol = SSM@p)
         S.innov <- .stdRepres(SSM@Q, SSM@p, SSM@p, withTest = TRUE,
                             Tn = Tn, time = time(SSM@time)[2:Tn])
         S.obs <- .stdRepres(SSM@V, SSM@q, SSM@q, withTest = TRUE,
                                Tn = Tn, time = time(SSM@time)[2:Tn])

         m.init <- as.numeric(.stdRepres.vec(SSM@a, SSM@p, Tn = 2,
                                    time = time(SSM@time)[1]))
         m.innov <- .stdRepres.vec(numeric(SSM@p), SSM@p, Tn = Tn,
                                   time = time(SSM@time)[2:Tn])

         m.obs <- .stdRepres.vec(numeric(SSM@q), SSM@q, Tn = Tn,
                                time = time(SSM@time)[2:Tn])

         new("SSMellDistribution.f",
              r.init = r.init, r.innov = r.innov, r.obs = r.obs,
              m.init = m.init, m.innov = m.innov, m.obs = m.obs,
              S.init = S.init, S.innov = S.innov, S.obs = m.obs)
}

SSMContDistribution.f <- function(SSM,
         r.init = mvrnorm, r.innov = mvrnorm, r.obs = mvrnorm,
         m.init = numeric(SSM@p), m.innov = numeric(SSM@p),
         m.obs = numeric(SSM@q),
         S.init = diag(SSM@p), S.innov = diag(SSM@p), S.obs = diag(SSM@q)){
         SSM <- makeArrayRepresentation(SSM)
         Tn <- length(SSM@time)

         S.init <- matrix(.stdRepres(S.init, SSM@p, SSM@p, withTest = TRUE,
                               Tn = 2, time = time(SSM@time)[1]),
                     nrow = SSM@p, ncol = SSM@p)
         S.innov <- .stdRepres(S.innov, SSM@p, SSM@p, withTest = TRUE,
                             Tn = Tn, time = time(SSM@time)[2:Tn])
         S.obs <- .stdRepres(S.obs, SSM@q, SSM@q, withTest = TRUE,
                                Tn = Tn, time = time(SSM@time)[2:Tn])

         m.init <- as.numeric(.stdRepres.vec(m.init, SSM@p, Tn = 2,
                                    time = time(SSM@time)[1]))
         m.innov <- .stdRepres.vec(m.innov, SSM@p, Tn = Tn,
                                   time = time(SSM@time)[2:Tn])

         m.obs <- .stdRepres.vec(m.obs, SSM@q, Tn = Tn,
                                time = time(SSM@time)[2:Tn])

         new("SSMellDistribution.f",
              r.init = r.init, r.innov = r.innov, r.obs = r.obs,
              m.init = m.init, m.innov = m.innov, m.obs = m.obs,
              S.init = S.init, S.innov = S.innov, S.obs = S.obs)
}

SSMConvDistribution.f<- function(idDistr, contDistr = idDistr, r.IO = 0, r.AO = 0){
       new("SSMConvDistribution.f", ideal = idDistr, cont = contDistr,
            r.IO = r.IO, r.AO = r.AO)
}

SSMwithDistribution <- function(SSM, Distr,
                       r.init = mvrnorm, r.innov = mvrnorm, r.obs = mvrnorm){
             if(missing(Distr))
                Distr <- new("SSMwithDistribution", SSM = SSM, Distribution =
                              SSMellDistribution.f( SSM, r.init = r.init,
                              r.innov = r.innov, r.obs = r.obs))
             new("SSMwithDistribution", SSM = SSM, Distribution = Distr)}
             
SSMwithConvDistribution <- function(SSM, Distr, Distrc, r.IO = 0, r.AO = 0,
                       r.init = mvrnorm, r.innov = mvrnorm, r.obs = mvrnorm,
                       rc.init = mvrnorm, rc.innov = mvrnorm, rc.obs = mvrnorm,
                       mc.init = numeric(SSM@p), mc.innov = numeric(SSM@p),
                       mc.obs = numeric(SSM@q),
                       Sc.init = diag(SSM@p), Sc.innov = diag(SSM@p),
                       Sc.obs = diag(SSM@q)){
             if(missing(Distr))
                Distr = SSMellDistribution.f( SSM, r.init = r.init,
                           r.innov = r.innov, r.obs = r.obs)
             if(missing(Distrc))
                Distrc = SSMContDistribution.f(SSM,
                           r.init = rc.init, r.innov = rc.innov, r.obs = rc.obs,
                           m.init = mc.init, m.innov = mc.innov, m.obs = mc.obs,
                           S.init = Sc.init, S.innov = Sc.innov, S.obs = Sc.obs
                                              )
             
             new("SSMwithConvDistribution", SSM = SSM,
                  Distribution = SSMConvDistribution.f(idDistr = Distr, 
                  contDistr = Distrc, 
                  r.IO = r.IO,
                  r.AO = r.AO))
             }
