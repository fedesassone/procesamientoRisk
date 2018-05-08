#leer archivo

dir = ''
dato.rr = read.csv(paste( dir , '10068.MIB.csv',sep=''))
print(names(dato.rr))
# "t" "RR"
plot(dato.rr $t,dato.rr $RR,type='l', ylab='RR (ms)',xlab='tiempo (s)')
points(dato.rr $t,dato.rr $RR,pch=20)

#parte 2

plot(dato.rr$t,dato.rr$RR,type='l', ylab='RR (ms)',xlab='tiempo
     (s)', xlim=c(237,247),ylim=c(600,1200))
points(dato.rr$t,dato.rr$RR,pch=20)
N = length(dato.rr$t)
for(j in 1:N)
{
  abline (v=dato.rr$t[ j ], lty =2)
}
Nmuestro = as.integer(2*max(dato.rr$t))
RRlineal = approx(dato.rr$t,dato.rr$RR,n=Nmuestro)
RRspline = spline(dato.rr$t,dato.rr$RR,n=Nmuestro)
lines (RRlineal$x, RRlineal$y, col='red')
points(RRlineal$x, RRlineal$y,pch=20,col='red')
lines (RRspline$x,RRspline$y, col='blue')
points(RRspline$x,RRspline$y,pch=20,col='green')

#parte 3

fft.RRlineal = Mod(fft(RRlineal$y))
fft.RRspline = Mod(fft(RRspline$y))
tiempo = 0:(length( fft.RRlineal )-1)
FrecuenciaMuestreo = 2
DeltaFrecMuestreo = FrecuenciaMuestreo/N
Frecuencia = DeltaFrecMuestreo*tiempo
op <- par(mfrow = c(1, 1))
plot(Frecuencia, fft.RRlineal , type='l' , xlim=c(0,0.2), ylim=c(0,30000),col='red')
lines (Frecuencia, fft.RRspline, col='green')

#muestras perdidas

dir = ''
dato.rr = read.csv(paste( dir , '10068Filtrado.csv',sep=''))
print(names(dato.rr))
# "t" "RR"
plot(dato.rr $t,dato.rr $RR,type='l', ylab='RR (ms)',xlab='tiempo (s)')
points(dato.rr $t,dato.rr $RR,pch=20)


library (muStat)
N = 48
tiempo = 0:(N-1)
st1 = rnorm(N,mean=1,sd=0.5)
indices = runif(25,min=5,max=45)
st1[ indices ] = NA
st11 = st1[ is.number(st1)]
tiempo1 = tiempo[is.number(st1)]
plot(tiempo1,st11, type='l')
st2 = spline(tiempo1,st11,N)
plot(tiempo,st1, type='b')
lines (st2$x, st2$y, col='red', lty =2)
points(st2$x, st2$y,pch=20,col='red')