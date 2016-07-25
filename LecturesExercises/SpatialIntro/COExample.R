library( fields)

#  colorado climate data 
  data(COmonthlyMet)
 
  x<- CO.loc
  y<- CO.tmin.MAM.climate
  elev<- CO.elev
  good<- !is.na( y)
  x<- x[good,]
  y<- y[good]
  elev<- elev[good]
 
    NGRID <- 50 
 # get elevations on a grid (will use these later) 
   COGrid<- fields.x.to.grid( x, nx=NGRID, ny=NGRID)
   COGridPoints<- make.surface.grid( COGrid)
   data( RMelevation)
   COElevGrid<- interp.surface( RMelevation, COGridPoints )
  
# take a look at the data
  quilt.plot( x, y) 
  US( add=TRUE)
  
  plot( elev, y)
  plot( x[,2], y)
  
  X<- cbind( x, elev) 
# 
 lmObj<- lm( y ~ lon+lat +elev, data=X )  
 summary( lmObj)
  quilt.plot( x, lmObj$residuals) 
  US( add=TRUE)

# easy fit using a thin plate spline
   fit0E<- Tps( x,y, Z=elev- mean(elev))
   
   
# fit a Kriging estimator Matern covariance smoothness =1.0
# range and nugget estimated by maxmimum likelihood
  fit1<- spatialProcess( x,y, ngrid=30)
  set.panel(2,2)
  plot( fit1)
  
  surface( fit1)
  US( add=TRUE)
  
  # take a look at residuals
  plot( elev, fit1$residuals)
  lmObj<- lm( fit1$residuals ~ elev)
  abline( lmObj, col="red", lwd=3)
  
  
  
# with elevations  
  fit1E<- spatialProcess( x,y, Z= elev, ngrid=30)
  
   sur0<- predictSurface( fit1E, nx=NGRID, ny=NGRID, Z= COElevGrid)
   image.plot( sur0, col=topo.colors(256))
    
   sur0Smooth<- predictSurface( fit1E, nx=NGRID, ny=NGRID, drop.Z= TRUE)
   image.plot( sur0Smooth)
   
   sur0SE<- predictSurfaceSE( fit1E, nx=NGRID, ny=NGRID, drop.Z= TRUE)
   image.plot( sur0SE)
   
   
# uncertainty 40 draws from conditional distribution
  set.seed(123)
  # next commmand takes a minute or so
  SEout<- sim.Krig( fit1E, xp=COGridPoints, Z= COElevGrid,
                     M= 40, drop.Z=TRUE)

 
  set.panel( 3,3)                            
  image.plot( sur0Smooth, 
              zlim=c(3.5,14.5), col=tim.colors(256))
  contour( sur0Smooth, levels= 9, lwd=3, col="grey", add=TRUE) 
  par( mar=c(3,3,1,1))  
  for( k in 1:8){
      image( as.surface( COGridPoints, SEout[k,]), 
               zlim =c(3.5,14.5),col=tim.colors(256), axes=FALSE)
      contour( as.surface( COGridPoints, SEout[k,]),
               lwd=3, col="grey",level=9, add=TRUE)
               title( k, adj=0, cex=2)
  }
  
  surSE<- apply( SEout, 2, sd )
 
   image.plot( as.surface( COGridPoints, surSE))
   points( x, col="magenta", pch=16) 
   contour( as.surface( COGridPoints,COElevGrid ), 
                level= c(2000, 3000), add= TRUE, col="grey30", lwd=3)
   US( add=TRUE, col="grey", lwd=2)
   

   
  
  
  
 
