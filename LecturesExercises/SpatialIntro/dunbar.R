
dunbar.colors<- function(N){
C3<- c(126, 72,61)
C1<- c( 162,122,47) + 50
C2<- c( 145,90,26) 
C4<- c( 111,22,6)
C5 <- c( 78,5,3)
C6<- c( 35,10,12)

hold<- rbind( C1,C2,C3, C4,C5,C6)
hold <- hold 
 hold2<- rgb( hold[,1], hold[,2], hold[,3],
      maxColorValue= 255)

    return(
        designer.colors(N, col=hold2 )
    )
}

obj<- outer( 1:100, 1:100, "+")
image.plot(obj,
          col=dunbar.colors(256))
