#' inside.bookvarlabel gives a label for a names of variables specified in the associated bookvar.
#' Used for output like tables or graphics.
#' @title Function \code{inside.bookvarlabel}
#' @rdname inside.bookvarlabel
#' @aliases inside.bookvarlabel
#' @export inside.bookvarlabel
#' @keywords dashboard, automatization, CEPOIwork, Clean release
#' @param x a vector of character. \code{x} containing the names of the variable in the associated bookvar.
#' @param bookvar a dataframe. \code{bookvar} contain the information needed for the variables.
#' @param ntry an integer : Number of the try if several. Cf SIMULRUN
#' @return a vector containing labels for each variable names.
inside.bookvarlabel <- function(x , bookvar , ntry ){
          y <- bookvar$label[ which(bookvar$vars == x) ]
          if ( length( y )== 0 ){
                    y <- bookvar$label[ which(bookvar$vars == sub( paste("_",ntry,sep="") , "_#n" , x ) ) ]
                    y <- sub( "#n" , ntry ,y )
          }
           return(y)
}

#' Bookvarlabel gives a label for a names of variables specified in the associated bookvar.
#' Used for output like tables or graphics.
#' @title Function \code{Bookvarlabel}
#' @rdname Bookvarlabel
#' @aliases Bookvarlabel
#' @export Bookvarlabel
#' @keywords dashboard, automatization, CEPOIwork, Clean release
#' @param x a vector of character. \code{x} containing the names of the variable in the associated bookvar.
#' @param bookvar a dataframe. \code{bookvar} contain the information needed for the variables.
#' @param ntry an integer : Number of the try if several. Cf SIMULRUN
#' @return a vector containing labels for each variable names.
#' @seealso \code{ Bookmodlabel}
Bookvarlabel <- function(x , bookvar, ntry=NULL){
                    ret <-  unlist(lapply(x, FUN=function(x)( return( inside.bookvarlabel( x, bookvar,ntry))) ))
          return(ret)
}

