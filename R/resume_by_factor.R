#' resume_by_factor give a description for variables by a factor
#' @title Function \code{resume_by_factor}
#' @rdname resume_by_factor
#' @aliases resume_by_factor
#' @export resume_by_factor
#' @keywords dashboard, automatization, CEPOIwork, Clean release, SIMULRUN, Description globale
#' @param tab a dataframe containing the data.
#' @param idcol a vector containing index of the variables to describe
#' @param name_group_var a character name of the factor
#' @param bookvarDispo a logical TRUE = bookvar is dispo
#' @param bookvar a dataframe containing infomation of the variable, if NULL the parametes will be ignored
#' @param bookmod a dataframe containing infomation of modalities of the variable, if NULL the parametes will be ignored
resume_by_factor <- function( tab, idcol,name_group_var,bookvarDispo = TRUE,bookvar,bookmod){

group_var <- factor( data[,which(colnames(data) == name_group_var)])
group_col <- which(colnames(tab) == name_group_var)



Tab <- resume_binary1(tab = tab[ which(tab[,group_col] == levels(group_var)[1]), ],idcol=idcol,bookvarDispo = bookvarDispo,bookvar = bookvar)[c(1,3)]
colnames(Tab) <- c( Bookmodlabel( levels(group_var)[1] , bookmod = bookmod , vars = name_group_var) ,"%")
for( i in 2:nlevels(group_var) ){
          temp <- resume_binary1(tab =  tab[ which(tab[,group_col] == levels(group_var)[i]), ],idcol=idcol,bookvarDispo = bookvarDispo,bookvar = bookvar)[c(1,3)]
          colnames(temp) <- c( Bookmodlabel( levels(group_var)[i] , bookmod = bookmod , vars = name_group_var) ,"%")
          Tab <- cbind( Tab,  temp)
}

temp1 <- resume_factor1( tab = tab[which(is.na( tab[,group_col]) == FALSE),] , idcol =group_col , bookvar=bookvar , bookmod = bookmod, vars_name = FALSE )[,c(1,3)]
temp <- c()
for( i in 1:nlevels(group_var) ){
          temp <- c(temp,temp1[i,])
}
cTab <- colnames(Tab)
Tab <- rbind(unlist(temp),Tab)
rownames(Tab)[1]<- "Tous"

return(Tab)
}
