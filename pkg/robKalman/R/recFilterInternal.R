writeMat <- function (left, right, i, opt) 
{
###########################################
##
##  R-function: writeMat - write a matrix to an larger array
##  author: Bernhard Spangl
##  version: 0.1 (2009-07-22)
##
###########################################

##  Paramters:
##  left ... array, e.g., [pd, pd, tt]
##  right ... matrix, e.g., [pd, pd]
##  i ... time parameter
##  opt ... logical, should the stuff really be saved? 

    if (opt) {
        left[, , i] <- right
        left
    } else {
        invisible(NULL)
    } 

}

writeArr <- function (left, right, i, opt) 
{
###########################################
##
##  R-function: writeArr - write an array to an larger array
##  author: Bernhard Spangl
##  version: 0.1 (2009-07-22)
##
###########################################

##  Paramters:
##  left ... array, e.g., [pd, pd, runs, tt]
##  right ... array, e.g., [pd, pd, runs]
##  i ... time parameter
##  opt ... logical, should the stuff really be saved? 

    if (opt) {
        left[, , , i] <- right
        left
    } else {
        invisible(NULL)
    } 

}

.writing <- function (type) 
{
###########################################
##
##  R-function: .writing - switches to appropriate writing function
##                         (internal function)
##  author: Bernhard Spangl
##  version: 0.1 (2009-07-22)
##
###########################################

##  Paramters:
##  type ... which writing function, for matrices or arrays 

    switch(type, mat = get("writeMat", mode="function"), 
                 arr = get("writeArr", mode="function"))

}

WriteRecF <- function (CovRunDep) 
{
###########################################
##
##  R-function: WriteRecF - returns appropriate writing function
##  author: Bernhard Spangl
##  version: 0.1 (2009-07-22)
##
###########################################

##  Paramters:
##  CovRunDep: logical, are there different prediction 
##             and filter error covariances for each simulated path, 
##             i.e., 'runs' > 1, as e.g. in the mACMfilter (-> complete 
##             vectorization may not be possible!)

    if (CovRunDep) {
        .writing(arr)
    } else {
        .writing(mat)
    }

}

