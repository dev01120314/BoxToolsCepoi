#' resume_group give for a classe and subclasse of variable description and group comparison
#' @title Function \code{resume_group}
#' @rdname resume_group
#' @aliases resume_group
#' @export resume_group
#' @keywords dashboard, automatization, CEPOIwork, Clean release, SIMULRUN, Description globale
#' @param data a dataframe containing the data.
#' @param bookvar a dataframe containing infomation of the variable, if NULL the parametes will be ignored
#' @param group1 vector containing indexes of the first group
#' @param group2 vector containing indexes of the second group
#' @param group vector 1 0
#' @param classe : name of the classe
#' @param ss_classe : name of the subclass
#' @return a vector containing labels for each variable names.
#' @param bookvarDispo logical if there is a bookvar
#' @seealso \code{resume_numeric}
resume_group <- function( data, bookvar, group1, group2,group, classe=NULL, ss_classe="",bookvarDispo = TRUE){
          if( length(classe )> 0){
          indexb <- which( bookvar$format_stat == "numeric" & bookvar$tableau_analyse == 1 & bookvar$Classe == classe & bookvar$sclasse_1 == ss_classe )
           }else{
          indexb <- which( bookvar$format_stat == "numeric" & bookvar$tableau_analyse == 1  )
           }
          indexd <- which( colnames(data) %in% bookvar$vars[indexb])
          if( length(indexd)> 0 ){
                    tab_all_num <- resume_numeric1( tab = data , idcol=indexd , bookvar=bookvar,bookvarDispo = bookvarDispo)
                    tab_nov_num <- resume_numeric1( tab = data[group1,] , idcol=indexd , bookvar=bookvar,bookvarDispo = bookvarDispo)
                    tab_exp_num <- resume_numeric1( tab = data[group2,] , idcol=indexd , bookvar=bookvar,bookvarDispo = bookvarDispo)
                    # Test de student
                    if( length(indexd) > 1 ){
                              index_na <- apply( X= data[,indexd], MARGIN=2, FUN <- function(x)( min(length(group1) -sum(is.na(x[group1])), length(group2) - sum(is.na(x[group2]))) ))
                              if( any( index_na < 2)){ indexd <- indexd[which(index_na >= 2)]}
                              index_cst <- apply( X= data[,indexd], MARGIN=2, FUN <- function(x)( length(unique(x)) ))
                              if( any( index_cst == 1)){ indexd <- indexd[which(index_cst > 1)]}
                    }else{

                    if( min( length(group1) - sum(is.na( data[group1,indexd] )), length(group2) - sum(is.na( data[group2,indexd] )))  <2 ){ indexd <- c() }
                    }
          }else{
                    tab_all_num <- c()
                    tab_nov_num <- c()
                    tab_exp_num <- c()
                    # Test de student
                    tests_num <- c()
                    tests_stat_num <- c()
                    tests_method_num <- c()
          }

          if( length(indexd)> 0 ){
                    if( length(indexd) > 1 ){
                              tests_num <- apply( X= data[,indexd], MARGIN=2, FUN <- function(x)(t.test(x[group1],x[group2])$p.value) )
                              tests_stat_num <- apply( X= data[,indexd], MARGIN=2, FUN <- function(x)(t.test(x[group1],x[group2])$statistic) )
                    }else{
                              tests_num <- t.test(data[group1,indexd],data[group2,indexd])$p.value
                              tests_stat_num <- t.test(data[group1,indexd],data[group2,indexd])$statistic
                              names(tests_num)  <- colnames(data)[indexd]

                    }
                    tests_method_num <- rep( "Student", length(tests_num))
          }else{
                    tab_all_num <- c()
                    tab_nov_num <- c()
                    tab_exp_num <- c()
                    # Test de student
                    tests_num <- c()
                    tests_stat_num <- c()
                    tests_method_num <- c()
          }
          # Binaire
          if( length(classe )> 0){
                    indexb <- which( bookvar$format_stat == "binary" & bookvar$tableau_analyse == 1 & bookvar$Classe == classe & bookvar$sclasse_1 == ss_classe )
          }else{
                    indexb <- which( bookvar$format_stat == "binary" & bookvar$tableau_analyse == 1  )
          }
          indexd <- which( colnames(data) %in% bookvar$vars[indexb])
          if( length(indexd) > 0 ){
                    tab_all_bi <- resume_binary1( tab = data , idcol=indexd , bookvar=bookvar,bookvarDispo = bookvarDispo)
                    tab_nov_bi <- resume_binary1( tab = data[group1,] , idcol=indexd , bookvar=bookvar,bookvarDispo = bookvarDispo)
                    tab_exp_bi <- resume_binary1( tab = data[group2,] , idcol=indexd , bookvar=bookvar,bookvarDispo = bookvarDispo)

                    if( length(indexd) > 1 ){
                              index_na <- apply( X= data[,indexd], MARGIN=2, FUN <- function(x)( min(length(group1) -sum(is.na(x[group1])), length(group2) - sum(is.na(x[group2]))) ))
                              if( any( index_na < 2)){ indexd <- indexd[which(index_na>=2)]}
                    }else{
                              if( min(length(group1) -sum(is.na( data[group1,indexd] )),length(group2) - sum(is.na( data[group2,indexd] )))  <2 ){ indexd <- c() }
                    }

                    if( length(indexd) > 1 ){
                              tests_bi <- apply( X= data[,indexd], MARGIN=2, FUN <- function(x)(
                                        switch( as.character( (length( unique( x )) <= 1) | ( any(is.na(x)) & (length( unique( x )) == 2) ) ) , "TRUE" = NaN, "FALSE"= chisq.test(x,group)$p.value) )
                                        )
                              tests_stat_bi <- apply( X= data[,indexd], MARGIN=2, FUN <- function(x)(
                                                      switch( as.character( (length( unique( x )) <= 1) | ( any(is.na(x)) & (length( unique( x )) == 2) ) ) , "TRUE" = NaN, "FALSE"=  chisq.test(x,group)$statistic) )
                                                      )
                    }else{
                              x <- data[,indexd]
                               tests_bi <- switch( as.character( (length( unique( x )) <= 1) | ( any(is.na(x)) & (length( unique( x )) == 2) ) ) , "TRUE" = NA, "FALSE"=   chisq.test(x,group)$p.value )
                               tests_stat_bi <- switch( as.character( (length( unique( x )) <= 1) | ( any(is.na(x)) & (length( unique( x )) == 2) ) ) , "TRUE" = NA, "FALSE"=   chisq.test(x,group)$p.value )
                               names(tests_bi)  <- colnames(data)[indexd]
                    }
                    tests_method_bi <- rep( "Chi2", length(tests_bi))
          }else{
                    tab_all_bi <- c()
                    tab_nov_bi <- c()
                    tab_exp_bi <- c()
                    # Test de student
                    tests_bi <- c()
                    tests_stat_bi <- c()
                    tests_method_bi <- c()
          }
          # Factor
          if( length(classe )> 0){
                    indexb <- which( bookvar$format_stat =="factor" & bookvar$tableau_analyse == 1 & bookvar$Classe == classe & bookvar$sclasse_1 == ss_classe )
          }else{
                    indexb <- which( bookvar$format_stat == "factor" & bookvar$tableau_analyse == 1  )
          }
          indexd <- which( colnames(data) %in% bookvar$vars[indexb])
          if( length(indexd) > 0 ){
          tab_all_fac <- resume_factor1( tab = data , idcol = indexd , bookvar = bookvar,bookvarDispo = bookvarDispo)
          tab_nov_fac <- resume_factor1( tab = data[ group1, ] , idcol = indexd , bookvar = bookvar,bookvarDispo = bookvarDispo)
          tab_exp_fac <- resume_factor1( tab = data[ group2, ] , idcol = indexd , bookvar = bookvar,bookvarDispo = bookvarDispo)

          if( length(indexd) > 1 ){
                    index_na <- apply( X= data[,indexd], MARGIN=2, FUN <- function(x)( min(length(group1) -sum(is.na(x[group1])), length(group2) - sum(is.na(x[group2]))) ))
                    if( any( index_na < 2)){ indexd <- indexd[which(index_na >= 2)]}
          }else{
                    if( min(length(group1) -sum(is.na( data[group1,indexd] )),length(group2) - sum(is.na( data[group2,indexd] )))  <2 ){ indexd <- c() }
          }

          if( length(indexd) > 1 ){
          tests_fac <- apply( X = data[,indexd], MARGIN = 2, FUN = function(x)(
                    switch( as.character( (length( unique( x )) <= 1) | ( any(is.na(x)) & (length( unique( x )) == 2) ) ) ,
                            "TRUE" = NaN, "FALSE"= chisq.test(table(x,group))$p.value) )
          )
          tests_stat_fac <- apply( X= data[,indexd], MARGIN=2, FUN <- function(x)(
                    switch( as.character( (length( unique( x )) <= 1) | ( any(is.na(x)) & (length( unique( x )) == 2) ) ) ,
                            "TRUE" = NaN, "FALSE"= chisq.test(table(x,group))$statistic) )
          )
                              }else{
                    x <- data[,indexd]
                    tests_fac <-  switch( as.character( (length( unique( x )) <= 1) | ( any(is.na(x)) & (length( unique( x )) == 2) ) ) ,
                                          "TRUE" = NULL, "FALSE"= chisq.test(table(x,group))$p.value)
                    tests_stat_fac <- switch( as.character( (length( unique( x )) <= 1) | ( any(is.na(x)) & (length( unique( x )) == 2) ) ) ,
                                             "TRUE" = NULL, "FALSE"= chisq.test(table(x,group))$statistic)
                    names(tests_fac)  <- colnames(data)[indexd]
          }
          tests_method_fac <- rep( "Chi2", length(tests_fac))
          }else{
                    tab_all_fac <- c()
                    tab_nov_fac <- c()
                    tab_exp_fac <- c()
                    # Test de student
                    tests_fac <- c()
                    tests_stat_fac <- c()
                    tests_method_fac <- c()
          }
          tests <-  c(tests_num,tests_bi,tests_fac)
          rtests <- c( names(tests_num), names( tests_bi), names(tests_fac))
          tests <-  data.frame( (tests) )
          if( length(tests) > 1 & bookvarDispo == TRUE) {
          rtests  <- Bookvarlabel( rtests , bookvar = bookvar )
          }
          tests_stat <-  c(tests_stat_num,tests_stat_bi,tests_stat_fac)
          tests_method <-  c(tests_method_num,tests_method_bi,tests_method_fac)
          tests$stat <- tests_stat
          tests$method <- tests_method
          colnames(tests)[1]<- "P.value"
          rownames(tests) <- rtests
          ret <- list( tab_all_num , tab_nov_num , tab_exp_num  ,tab_all_bi , tab_nov_bi , tab_exp_bi , tab_all_fac, tab_nov_fac , tab_exp_fac , tests)
return(ret)
}
