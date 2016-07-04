#' BoxToolsCepoi
#'
#' \strong{ A tools box } for descriptive statistics.
#' @docType package
#' @name BoxToolsCepoi
NULL

#' @import stats
#' @import abind
#' @import grid
#' @import gridExtra
NULL

#' resume_binary1 gives a summary of indicator for binary outcomes
#' @title Function \code{resume_binary1}
#' @rdname resume_binary1
#' @aliases resume_binary1
#' @export resume_binary1
#' @keywords dashboard, automatization, CEPOIwork, Clean release
#' @param tab a matrix or dataframe containing the data.
#' @param idcol a numeric vector containting column's index(es) to treat
#' @param bookvarDispo logical if there is a bookvar
#' @param bookvar a dataframe containing infomation of the variable, if NULL the parametes will be ignored
#' @return a vector containing labels for each variable names.
#' @seealso \code{resume_numeric1}
resume_binary1 <- function(tab , idcol, bookvarDispo = TRUE, bookvar ){
  if ( length( idcol ) > 1) {
    ret <- data.frame( N_TRUE = apply( X = tab[,idcol] ,
                                       MARGIN = 2 ,
                                       FUN = function(x )(sum( x == 1 , na.rm = TRUE) )
    ),
    N_FALSE = apply( X = tab[,idcol] ,
                     MARGIN = 2 ,
                     FUN = function(x )(sum( x == 0 , na.rm = TRUE) )
    ),
    Per_TRUE = apply( X = tab[,idcol] ,
                      MARGIN = 2 ,
                      FUN = function(x )(100 * sum( x == 1 , na.rm = TRUE) / length( x ) )
    ),
    Per_FALSE = apply( X = tab[,idcol] ,
                       MARGIN = 2 ,
                       FUN = function(x )(100 * sum( x == 0 , na.rm = TRUE) / length( x ) )
    ),
    N_NA = apply( X = tab[,idcol] ,
                  MARGIN = 2 ,
                  FUN = function(x )(sum( is.na( x ) ) )
    ),
    Per_NA = apply( X = tab[,idcol] ,
                    MARGIN = 2 ,
                    FUN = function(x )(100 * sum( is.na( x ) ) / length( x )  )
    )

    )
  }else{
    ret <- t(data.frame( c(  N_TRUE = sum( tab[,idcol] == 1 , na.rm = TRUE),
                             N_FALSE = sum( tab[,idcol] == 0 , na.rm = TRUE),
                             Per_TRUE = 100 * sum( tab[,idcol] == 1 , na.rm = TRUE) / length( tab[,idcol] ),
                             Per_FALSE =  100 * sum( tab[,idcol] == 0 , na.rm = TRUE) / length( tab[,idcol] ) ,
                             N_NA = sum( is.na( tab[,idcol] ) ) ,
                             Per_NA = 100 * sum( is.na( tab[,idcol] ) ) / length( tab[,idcol] )
    )))
  }
  if ( bookvarDispo == TRUE) {
    rownames(ret) = Bookvarlabel( colnames(tab)[idcol], bookvar = bookvar)
  }else{
    rownames(ret) = colnames(tab)[idcol]
  }

  colnames( ret )  <- c("Nb Mod.","Nb non Mod.", "% Mod.","% non Mod.","Nb NA","% NA")
  return( ret )
}
