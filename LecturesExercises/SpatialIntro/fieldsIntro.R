library(fields)

data( COmonthlyMet)

x<- CO.loc
y<- CO.tmin.MAM.climate
good<- !is.na(y)

x<- x[good,]
y<- y[good]

plot( x)
US( add=TRUE)

quilt.plot( x, y)

obj<- Tps( x,y)

obj2<- Tps(x,y, df=150)
out2<- predictSurface(obj2)
obj3<- Tps(x,y, df=20)


# a 1 d example and derivatives

plot( rat.diet$t, rat.diet$trt)

obj<- Tps( as.matrix(rat.diet$t ),
           as.matrix( rat.diet$trt))

lines( obj$x, obj$fitted.values)

xg<- seq( 0,100,length.out=5000)
yg<- predict( obj, x=xg)

plot( xg[-1], diff( yg), type="l", col="orange")


elev<- CO.elev[good]

obj4<- Tps( x,y, Z=elev )









