#leer archivo

dir = ''
dato.rr = read.csv(paste( dir , '10068.MIB.csv',sep=''))
print(names(dato.rr))
# "t" "RR"
plot(dato.rr $t,dato.rr $RR,type='l', ylab='RR (ms)',xlab='tiempo (s)')
points(dato.rr $t,dato.rr $RR,pch=20)

#parte 2

plot(dato.rr$t,dato.rr$RR,type='l', ylab='RR (ms)',xlab='tiempo(s)', xlim=c(0,72473),ylim=c(0,8000))
points(dato.rr$t,dato.rr$RR,pch=20)
Nmuestro = as.integer(2*max(dato.rr$t))
RRlineal = approx(dato.rr$t,dato.rr$RR,n=Nmuestro)
RRspline = spline(dato.rr$t,dato.rr$RR,n=Nmuestro)
lines (RRlineal$x, RRlineal$y, col='red')
points(RRlineal$x, RRlineal$y,pch=20,col='red')
lines (RRspline$x,RRspline$y, col='blue')
points(RRspline$x,RRspline$y,pch=20,col='green')

#muestras perdidas

dir = ''
dato.rr = read.csv(paste( dir , '10068Filtrado.csv',sep=''))
print(names(dato.rr))
# "t" "RR"
plot(dato.rr $t,dato.rr $RR,type='l', ylab='RR (ms)',xlab='tiempo (s)')
points(dato.rr $t,dato.rr $RR,pch=20)


library (muStat)
N = 137857
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

