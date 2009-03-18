#setMethod("plot", "ParamFamily", 
#    function(x,y=NULL,...){ 
#        e1 <- x@distribution
#        if(!is(e1, "UnivariateDistribution")) stop("not yet implemented")
#
#        plot(e1) 
#    })
