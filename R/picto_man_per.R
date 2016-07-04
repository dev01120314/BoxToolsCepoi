#' picto_man_per drawn man colored by percentages
#' @title Function \code{picto_man_per}
#' @rdname picto_man_per
#' @aliases picto_man_per
#' @export picto_man_per
#' @keywords dashboard, automatization, CEPOIwork, Clean release
#' @param picto an array : the picto source used
#' @param per a numeric the percentage
#' @param Nb an integer the number
#' @param w1 a vector of 3 : the color 1
#' @param w2 a vector of 3 : the color 2
#' @param s an interger
#' @param fontsize an integer the fontsize
#' @return a plot .
  picto_man_per<- function(picto,per,Nb,w1 = c( 0 , 0.3 , 0.7 ) ,w2 = rep(0.7,3) , s = 5, fontsize=35 ){
     w <- c(0,.85,1)
    imgB <- picto[1:round(dim(picto)[1]*(1-per),0) ,,]
    imgC <-switch(as.character(per==0),"FALSE"=picto[(1+round(dim(picto)[1]*(1-per),0) ):dim(picto)[1] ,,], "TRUE"=NULL)
    for (i in seq_along( w2 )) ( imgB[ ,,i] <- imgB[,,i] * w2[i] )
    for (i in seq_along( w1 )) ( imgC[ ,,i] <- imgC[,,i] * w1[i] )
    picto<- abind(imgB,imgC,along=1)
    r <-( nrow(picto) / ncol(picto) )  # size ratio
    picto <- rasterGrob(picto)
    text_per<- textGrob(paste(round(per*100,0)," %"), x = unit(0, "npc"), y = unit(0.2, "npc"),
             just = "left", hjust = NULL, vjust = NULL, rot = 0,
             check.overlap = FALSE, default.units = "npc",
             name = NULL, gp = gpar(fontsize = fontsize , fontface="bold"), vp = NULL)
    text_Nb<-  textGrob(paste(Nb," ind"), x = unit(0, "npc"), y = unit(0.8, "npc"),
                just = "left", hjust = NULL, vjust = NULL, rot = 0,
                check.overlap = FALSE, default.units = "npc",
                name = NULL, gp = gpar(fontsize = fontsize , fontface="bold"), vp = NULL)
    lay <- rbind(c(1,1,2),
                c(1,1,3))
    picto <-  grid.arrange(grobs = list(picto,text_per,text_Nb), layout_matrix = lay)
     return(picto)
  }