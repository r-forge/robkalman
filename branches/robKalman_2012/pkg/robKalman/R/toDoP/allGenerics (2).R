if(!isGeneric("name"))
   setGeneric("name", function(object) standardGeneric("name"))

if(!isGeneric("name<-"))
   setGeneric("name<-", function(object, value) standardGeneric("name<-"))

if(!isGeneric("fct"))
   setGeneric("fct", function(object) standardGeneric("fct"))

if(!isGeneric("fct<-"))
   setGeneric("fct<-", function(object, value) standardGeneric("fct<-"))

if(!isGeneric("control"))
   setGeneric("control", function(object) standardGeneric("control"))

if(!isGeneric("control<-"))
   setGeneric("control<-", function(object, value) standardGeneric("control<-"))

if(!isGeneric("control"))
   setGeneric("control", function(object) standardGeneric("control"))

if(!isGeneric("control<-"))
   setGeneric("control<-", function(object, value) standardGeneric("control<-"))
