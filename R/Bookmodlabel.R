#' Bookmodlabel gives a label for modalities of a given variable specified in the associated bookmod.
#' Used for output like tables or graphics.
#' @title Function \code{Bookmodlabel}
#' @rdname Bookmodlabel
#' @aliases Bookmodlabel
#' @export Bookmodlabel
#' @keywords dashboard, automatization, CEPOIwork, Clean release
#' @param mod a vector of character. \code{x} containing modalities for the variable vars in the associated bookmod.
#' @param vars a charracter. \code{vars} contains the names of the variable
#' @param bookmod a dataframe. \code{bookmod} contain the information needed for the variables.
#' @return a vector containing labels for each variable names.
#' @seealso \code{ Bookvarlabel}
Bookmodlabel <- function(mod , vars, bookmod){
  Y <- bookmod[ which( bookmod$vars == vars),]
  ret <- unlist( lapply( mod, function(x)(Y$modlabel[ which( Y$mod %in% x  )])) )
  return(ret)
}


