#if(!isGeneric("type")){ 
#    setGeneric("type", function(object) standardGeneric("type"))
#}
#if(!isGeneric("center")){ 
#    setGeneric("center", function(object) standardGeneric("center"))
#}
#if(!isGeneric("center<-")){
#    setGeneric("center<-", function(object, value) standardGeneric("center<-"))
#}
#
