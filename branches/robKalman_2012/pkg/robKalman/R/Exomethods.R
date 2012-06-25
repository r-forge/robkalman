setMethod("createExo", "array", function (object)    # time-variant case, linear
{
##  Exo ... array of observation matrices, Exo[, , t]
##  T ... selection matrix array (observation noise)
    Exo <- object

    funcExo <- function (t, y, control, dots)
    {
    ##  t ... time index
    ##  y ... observation
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        return(Exo[,t])
    }

    return(new("FunctionWithControl",funcExo))
}

setMethod("createExo", "matrix", function (object)    # time-invariant case, linear
{
##  Exo ... observation matrix
##  T ... selection matrix (observation noise)
    Exo <- object

    funcExo <- function (t, x1, eps, w, control, dots)
    {
    ##  t ... time index
    ##  y ... observation
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        return(Exo)
    }

    return(new("FunctionWithControl",funcExo))
}

setMethod("createExo", "function", function (object)    # function case
{
##  Exo ... observation matrix
##  T ... selection matrix (observation noise)
    Exo <- object

    ### some Exo checking possible and needed

    funcExo <- function (t, x1, eps, w, control, dots)
    {
    ##  t ... time index
    ##  y ... observation
    ##  control ... control parameters, list
    ##  dots ... additional parameters, list
        return(Exo(t, y, control, dots))
    }

    return(new("FunctionWithControl",funcExo))
}
