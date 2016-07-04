#' Print a number with , and a fixed number of decimal
#' @title Function \code{Cfloor}
#' @rdname Cfloor
#' @aliases Cfloor
#' @export Cfloor
#' @keywords dashboard, automatization, CEPOIwork, Clean release
#' @param x a numeric \code{x}
#' @param prec an integer. \code{prec} is the precision set to 1
#' @return "a character
Cfloor <- function(x,prec=1){
 ret <- switch( as.character(prec == 0),
                "TRUE" = paste0(trunc(x)),
                "FALSE" = paste0(trunc(x),",",
         round( (x-trunc(x) )*10^prec,0)
  )
 )
 return(ret)
}

