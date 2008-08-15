.onLoad <- function(lib, pkg){
    require("methods", character = TRUE, quietly = TRUE)

}


.onAttach <- function(library, pkg)
{
buildStartupMessage(pkg="robKalman", library=library, packageHelp=TRUE #, 
                    #MANUAL=""
                    )
  invisible()
}

#.onUnload <- function(libpath)
#{
#    library.dynam.unload("distrEx", libpath)
#}
#
#
## register zoo as "S4"-class
setOldClass("zoo")
#
setClassUnion("Hyperparamtype", 
               c("NULL","matrix", "array", "zoo")
               )

#
#
#
#
## positive definite, symmetric matrices with finite entries
#setClass("ACMcontrol",representation())
#setClass("rLScontrol",representation())
#
#
#
#
# class SSM --- State space model
setClass("SSM",
          representation = representation(
                                name = "character",   ## name of the ssm
                                F = "Hyperparamtype", ## transition matrix/ces or NULL
                                Z = "Hyperparamtype", ## observation matrix/ces or NULL
                                Q = "Hyperparamtype", ## innovation covariance or NULL
                                V = "Hyperparamtype", ## observation error covariance or NULL
                                p = "numeric",  ## state dimension
                                q = "numeric"), ## observation dimension
          prototype = prototype(name = gettext("a state space"), 
                                F = NULL,
                                Z = NULL,
                                Q = NULL,
                                V = NULL,
                                p = 1, 
                                q = 1), 
          contains = "VIRTUAL")

# class TimeInvariantSSM 
setClass("TimeInvariantSSM",
          prototype = prototype(name = gettext("a time-invariant state space"), 
                                F = 1,
                                Z = 1,
                                Q = 1,
                                V = 1,
                                p = 1, 
                                q = 1), 
          contains = "SSM")          
          