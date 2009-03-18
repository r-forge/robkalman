.weighting <- function (type) 
{
###########################################
##
##  R-function: .weighting - switches to appropriate weight function
##                           (internal function)
##  author: Bernhard Spangl
##  version: 0.1 (2008-03-31)
##
###########################################

##  Paramters:
##  type ... which weight function, only Jacobian matrix of multivariate 
##           analogue of Hampel's psi-function available (default: "deriv") 

    switch(type, deriv = get("jacobian.Hampel", mode="function")) 
}

