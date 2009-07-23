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

