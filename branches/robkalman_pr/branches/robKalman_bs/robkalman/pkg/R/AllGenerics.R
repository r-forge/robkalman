############################################################################
# Access methods
############################################################################

if(!isGeneric("name")) 
    setGeneric("name", function(object) standardGeneric("name"))

if(!isGeneric("getp")) 
   setGeneric("getp", function(object) standardGeneric("getp"))
if(!isGeneric("getq")) 
   setGeneric("getq", function(object) standardGeneric("getq"))

if(!isGeneric("getF")) 
   setGeneric("getF", function(object,t) standardGeneric("getF"))
if(!isGeneric("getZ")) 
   setGeneric("getZ", function(object,t) standardGeneric("getZ"))
if(!isGeneric("getQ")) 
   setGeneric("getQ", function(object,t) standardGeneric("getQ"))
if(!isGeneric("getV")) 
   setGeneric("getV", function(object,t) standardGeneric("getV"))


############################################################################
# Replacement methods
############################################################################

if(!isGeneric("name<-")) 
    setGeneric("name<-", 
                function(object, value) standardGeneric("name<-"))

############################################################################
# generics to  "usual"  methods
############################################################################


# general methods

if(!isGeneric("isOldVersion")) 
   setGeneric("isOldVersion", function(object) standardGeneric("isOldVersion"))

if(!isGeneric("conv2NewVersion")) 
   setGeneric("conv2NewVersion", 
               function(object) standardGeneric("conv2NewVersion"))
### setting gaps

if(!isGeneric("setgaps"))
   setGeneric("setgaps", function(object, ...) standardGeneric("setgaps"))

#### generics for log, log10, lgamma, gamma


if(!isGeneric("log"))
   setGeneric("log") #, function(x, base) standardGeneric("log"))
if(!isGeneric("log10"))
   setGeneric("log10")
if(!isGeneric("lgamma"))
   setGeneric("lgamma")
if(!isGeneric("gamma"))
   setGeneric("gamma")


