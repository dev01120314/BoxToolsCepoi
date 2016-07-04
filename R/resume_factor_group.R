#' resume_factor_group gives a summary of indicator for factorial outcomes
#' @title Function \code{resume_factor_group}
#' @rdname resume_factor_group
#' @aliases resume_factor_group
#' @export resume_factor_group
#' @keywords dashboard, automatization, CEPOIwork, Clean release, SIMULRUN, Description globale
#' @param tab a matrix or dataframe containing the data.
#' @param idcol a numeric vector containting column's index(es) to treat
#' @param group vector with 0/1
#' @param bookvarDispo logical if there is a bookvar
#' @param bookvar a dataframe containing infomation of the variable, if NULL the parametes will be ignored
#' @param bookmod a dataframe : the bookmod
#' @return a vector containing labels for each variable names.
#' @seealso \code{resume_numeric}
resume_factor_group <- function(tab , idcol=seq( 1,dim(tab)[2],1), group, bookvarDispo=TRUE, bookvar, bookmod=NULL ){
  # New table :
  nottreat <- tab[,-idcol]

  nameRows <- colnames(tab)[idcol]
  nameRow <- colnames(tab)[idcol]
  if ( bookvarDispo == TRUE) {
    nameRow <- Bookvarlabel(nameRows, bookvar = bookvar)
  }
  tab <- tab[,idcol]
  tempX <- c()
  ctempX <- c()
  if (length(idcol ) == 1) {
    temp <- factor( tab )
    nameMode <- levels( temp )
    if( length(bookmod) > 0){
      nameMode <- Bookmodlabel( mod = as.numeric(nameMode), vars = nameRows, bookmod = bookmod)
    }
    if (nlevels(temp) > 1) {
      for (j in seq_along( levels(temp) )) {#j="21"
        tempX <- cbind( tempX, temp == levels(temp)[j] )

        ctempX <- c( ctempX, paste0(nameRow," : ",nameMode[j] ))
      }
    }else{
      tempX <- cbind( tempX, temp == levels(temp)[1] )
      ctempX <- c( ctempX,  paste0("_",nameMode[1]))
    }
  }else{
    for (i in seq(1,dim(tab)[2])) { # i=1
      temp <- factor( tab[,i] )
      nameMode <- levels( temp )
      if( length(bookmod) > 0){
        nameMode <- Bookmodlabel( mod = as.numeric(nameMode), vars = nameRows[i], bookmod = bookmod)
      }
      if ( nlevels(temp) > 1 ) {
        for ( j in seq_along( levels( temp ) ) ) { #j=2
          tempX <- cbind( tempX, temp ==  levels( temp )[j] )
          ctempX <- c( ctempX, paste0( nameRow[ i ]," : ",nameMode[ j ] ) )
        }
      }else{
        tempX <- cbind( tempX, temp == levels(temp)[1])
        ctempX <- c( ctempX,  paste0(nameRow[i],"_",nameMode[1]))
      }
    }}
  tab <- tempX
  colnames(tab) <- ctempX
  rm(temp)
  rm(ctempX)
  rm(tempX)
  if (dim( tab )[1]  > 1 ) {
    ret <- data.frame( N_TRUE = apply( X = tab ,
                                       MARGIN = 2 ,
                                       FUN = function(x )(sum(x == 1 , na.rm = TRUE) )
    ),
    N_FALSE = apply( X = tab ,
                     MARGIN = 2 ,
                     FUN = function(x )(sum(x == 0 , na.rm = TRUE) )
    ),
    Per_TRUE = apply( X = tab ,
                      MARGIN = 2 ,
                      FUN = function(x )(100 * sum(x == 1 , na.rm = TRUE) / length( x ) )
    ),
    Per_FALSE = apply( X = tab ,
                       MARGIN = 2 ,
                       FUN = function(x )(100 * sum(x == 0 , na.rm = TRUE) / length( x ) )
    ),
    N_NA = apply( X = tab ,
                  MARGIN = 2 ,
                  FUN = function(x )(sum(is.na( x ) ) )
    ),
    Per_NA = apply( X = tab ,
                    MARGIN = 2 ,
                    FUN = function(x )(100 * sum( is.na( x ) ) / length( x )  )
    )

    )
  }else{
    ret <- t(data.frame( c(  N_TRUE = sum( tab == 1 , na.rm = TRUE),
                             N_FALSE = sum( tab == 0 , na.rm = TRUE),
                             Per_TRUE = 100 * sum( tab == 1 , na.rm = TRUE) / dim( tab )[1],
                             Per_FALSE =  100 * sum( tab == 0 , na.rm = TRUE) / dim( tab )[1] ,
                             N_NA = sum( is.na( tab ) ) ,
                             Per_NA = 100 * sum( is.na( tab[,idcol] ) ) / dim( tab )[1]
    )))
    rownames(ret) <- colnames(tab)[ idcol ]
  }
  colnames( ret )  <- c("Nb Mod.","Nb non Mod.", "% Mod.","% non Mod.","Nb NA","% NA")
  return( ret )
}
