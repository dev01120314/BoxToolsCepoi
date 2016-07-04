#' resume_numeric1 gives a summary of indicator calculated over numeric variables
#' @title Function \code{resume_numeric1}
#' @rdname resume_numeric1
#' @aliases resume_numeric1
#' @export resume_numeric1
#' @keywords dashboard, automatization, CEPOIwork , SIMULRUN
#' @param tab a matrix or dataframe containing the data.
#' @param idcol a numeric vector containting column's index(es) to treat
#' @param indice a character : "Mean","Median", "Both"
#' @param bookvarDispo logical if there is a bookvar
#' @param bookvar a dataframe containing infomation of the variable, if NULL the parametes will be ignored
#' @return a vector containing labels for each variable names.
#' @seealso \code{resume_factor1}
resume_numeric1 <- function(tab , idcol , indice = "Both" ,bookvarDispo = TRUE, bookvar ){
          if ( indice == "Mean") {
                    if ( length( idcol ) > 1) {
                              ret <- data.frame( Mean = apply( X = tab[,idcol] , MARGIN = 2 , FUN = function(x )(mean(x , na.rm = TRUE) ) ),
                                                 Std = apply( X = tab[,idcol] , MARGIN = 2 , FUN = function(x )(sqrt( var(x , na.rm = TRUE) ) ) ),
                                                 Nb = apply( X = tab[,idcol] , MARGIN = 2 , FUN = function(x )(sum(is.na(x ) == FALSE ) ) ),
                                                 Per_NA = apply( X = tab[,idcol] , MARGIN = 2 , FUN = function(x )(sum(is.na(x ) )*100/length(x) ) )
                              )
                    }else{
                              ret <- t( as.data.frame( c(Mean =  mean(tab[,idcol] , na.rm = TRUE) ,
                                                         Std = sqrt( var( tab[,idcol] , na.rm = TRUE) ),
                                                         Nb =  sum( is.na( tab[,idcol] ) == FALSE ) ,
                                                         Per_NA =  sum( is.na(tab[,idcol] ) )*100/length( tab[,idcol] ) ) ))
                              rownames(ret) = colnames(tab)[idcol]
                    }
                    colnames( ret ) [ 3 ] <- "Nb"
                    colnames( ret ) [ 4 ] <- "% NA"
          }
          if ( indice == "Median" ) {
                    if ( length( idcol) > 1) {
                              ret = cbind( t( apply( X = tab[,idcol] , MARGIN = 2 , FUN = function(x )(quantile(x , na.rm = TRUE) ) ) ),
                                           Nb = apply( X = tab[,idcol] , MARGIN = 2 , FUN = function(x )(sum(is.na(x ) == FALSE ) ) ) ,
                                           Per_NA = apply( X = tab[,idcol] , MARGIN = 2 , FUN = function(x )(sum(is.na(x ) )*100/length(x) ) )
                              )
                    }else{
                              ret <- t( as.data.frame( c( quantile(tab[,idcol] , na.rm = TRUE) ,Nb =  sum(is.na( tab[,idcol] ) == FALSE ) ,
                                                          Per_NA = sum(is.na(tab[,idcol] ) )*100/length( tab[,idcol] ) ) ))
                              rownames(ret) = colnames(tab)[idcol]
                    }
          }
          if ( indice == "Both") {
                    if ( length( idcol ) > 1) {
                              ret <- data.frame( Mean = apply( X = tab[,idcol] , MARGIN = 2 , FUN = function(x )(mean(x , na.rm = TRUE) ) ),
                                                 Std = apply( X = tab[,idcol] , MARGIN = 2 , FUN = function(x )(sqrt( var(x , na.rm = TRUE) ) ) )
                              )
                              ret = cbind(ret ,cbind( t( apply( X = tab[,idcol] , MARGIN = 2 , FUN = function(x )(quantile(x , na.rm = TRUE) ) ) ),
                                                      Nb = apply( X = tab[,idcol] , MARGIN = 2 , FUN = function(x )(sum(is.na(x ) == FALSE ) ) ) ,
                                                      Per_NA = apply( X = tab[,idcol] , MARGIN = 2 , FUN = function(x )(sum(is.na(x ) )*100/length(x) ) )
                              ))
                    }else{
                              ret <- t( as.data.frame( c(Mean =  mean(tab[,idcol] , na.rm = TRUE),
                                                         Std = sqrt( var( tab[,idcol] , na.rm = TRUE) ),
                                                         quantile(tab[,idcol] , na.rm = TRUE),
                                                         Nb =  sum(is.na( tab[,idcol] ) == FALSE ) ,
                                                         Per_NA =  sum(is.na(tab[,idcol] ) )*100/length( tab[,idcol] ) ) ))
                              rownames(ret) = colnames(tab)[idcol]

                    }
                    colnames( ret ) [ 8 ] <- "Nb"
                    colnames( ret ) [ 9 ] <- "% NA"
          }
          if ( bookvarDispo == TRUE) {
                    rownames(ret) <- Bookvarlabel( rownames(ret) ,bookvar=bookvar)
          }
          return( ret )
}

