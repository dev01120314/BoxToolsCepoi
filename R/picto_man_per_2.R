#' #' picto_man_per_2 drawn a pictogram filled with two per1 et per2
#' @title Function \code{picto_man_per_2}
#' @rdname picto_man_per_2
#' @aliases picto_man_per_2
#' @export picto_man_per_2
#' @keywords dashboard, automatization, CEPOIwork, Clean release
#' @param picto an array : the picto source used
#' @param per1 a numeric the percentage 1
#' @param per2 a numeric the percentage 2
#' @param Nb1 an integer the number 1
#' @param Nb2 an integer the number 2
#' @param title1 a charater : the title 1
#' @param title2 a character : the tilte 2
#' @param w1 a vector of 3 : the color 1
#' @param w2 a vector of 3 : the color 2
#' @param s an interger
#' @param fontsize an integer the fontsize
#' @return a plot .
picto_man_per_2 <- function(picto,per1,Nb1,per2,Nb2,title1,title2,w1 = c( 0 , 0.3 , 0.7 ) , w2 = rep(0.7,3) , s = 5, fontsize=35 ){
  w <- c(0,.85,1)
  imgB1 <- switch(as.character( (1 - per1) == 0),"FALSE" = picto[1:round(dim(picto)[1]*(1  - per1),0) ,,], "TRUE" = NULL)
  imgC1 <- switch(as.character(per1 == 0),"FALSE" = picto[(1 + round(dim(picto)[1] * (1 - per1),0) ):dim(picto)[ 1 ] ,,], "TRUE" = NULL)
  for (i in seq_along( w2 )) (imgB1[ ,,i] <- imgB1[,,i] * w2[i] )
  for (i in seq_along( w1 )) (imgC1[ ,,i] <- imgC1[,,i] * w1[i] )
  picto1 <- abind( imgB1, imgC1, along = 1)
  r <- (nrow(picto1) / ncol(picto1) )  # size ratio
  picto1 <- rasterGrob(picto1)
  text_per1 <- textGrob(paste(round(per1*100,0)," %"), x = unit(0, "npc"), y = unit(0.2, "npc"),
                      just = "left", hjust = NULL, vjust = NULL, rot = 0,
                      check.overlap = FALSE, default.units = "npc",
                      name = NULL, gp = gpar(fontsize = fontsize , fontface = "bold"), vp = NULL)
  text_Nb1 <-  textGrob(paste(Nb1," ind"), x = unit(0, "npc"), y = unit(0.8, "npc"),
                      just = "left", hjust = NULL, vjust = NULL, rot = 0,
                      check.overlap = FALSE, default.units = "npc",
                      name = NULL, gp = gpar(fontsize = fontsize , fontface = "bold"), vp = NULL)
  title1 <-  textGrob(title1, x = unit(0.3, "npc"), y = unit(0, "npc"),
                      just = "top" , rot = 0,
                     check.overlap = FALSE, default.units = "npc",
                     name = NULL, gp = gpar(fontsize = fontsize , fontface = "bold"), vp = NULL)
  imgB2 <- switch(as.character( (1 - per2) == 0),"FALSE" =picto[1:round(dim(picto)[1]*(1 - per2),0) ,,], "TRUE" = NULL)
  imgC2 <- switch(as.character(per2 == 0),"FALSE" = picto[(1 + round(dim(picto)[1] * (1 - per2),0) ):dim(picto)[1] ,,], "TRUE" = NULL)
  for (i in seq_along( w2 )) (imgB2[ ,,i] <- imgB2[,,i] * w2[i] )
  for (i in seq_along( w1 )) (imgC2[ ,,i] <- imgC2[,,i] * w1[i] )
  picto2 <- abind(imgB2,imgC2,along = 1)
  r <- (nrow(picto2) / ncol(picto2) )  # size ratio
  picto2 <- rasterGrob(picto2)
  text_per2 <- textGrob(paste(round(per2*100,0)," %"), x = unit(0, "npc"), y = unit(0.2, "npc"),
                       just = "left", hjust = NULL, vjust = NULL, rot = 0,
                       check.overlap = FALSE, default.units = "npc",
                       name = NULL, gp = gpar(fontsize = fontsize , fontface = "bold"), vp = NULL)
  text_Nb2 <-  textGrob(paste(Nb2," ind"), x = unit(0, "npc"), y = unit(0.8, "npc"),
                       just = "left", hjust = NULL, vjust = NULL, rot = 0,
                       check.overlap = FALSE, default.units = "npc",
                       name = NULL, gp = gpar(fontsize = fontsize , fontface = "bold"), vp = NULL)
  title2 <-  textGrob(title2, x = unit(0.3, "npc"), y = unit(0, "npc"),
                     just = "top", rot = 0,
                     check.overlap = FALSE, default.units = "npc",
                     name = NULL, gp = gpar(fontsize = fontsize , fontface = "bold"), vp = NULL)
  lay <- rbind(c(1,1,7,4,4,8),
               c(1,1,2,4,4,5),
               c(1,1,3,4,4,6))
  picto <-  grid.arrange(grobs = list(picto1,text_per1,text_Nb1,picto2,text_per2,text_Nb2,title1,title2), layout_matrix = lay)
  return(picto)
}