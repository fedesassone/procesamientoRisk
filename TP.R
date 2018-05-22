#leer archivo

dir = ''
dato.rr = read.csv(paste( dir , '10068.MIB.csv',sep=''))
print(names(dato.rr))
# "t" "RR"
plot(dato.rr $t,dato.rr $RR,type='l', ylab='RR (ms)',xlab='tiempo (s)')
points(dato.rr $t,dato.rr $RR,pch=20)

#parte 2

plot(dato.rr$t,dato.rr$RR,type='l', ylab='RR (ms)',xlab='tiempo(s)', xlim=c(0,72473),ylim=c(0,20000))
points(dato.rr$t,dato.rr$RR,pch=20)
Nmuestro = as.integer(2*max(dato.rr$t))
RRlineal = approx(dato.rr$t,dato.rr$RR,n=as.integer(71377.58399999907/0.25))
RRspline = spline(dato.rr$t,dato.rr$RR,n=as.integer(71377.58399999907/0.25))
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


#parte 2

plot(dato.rr$t,dato.rr$RR,type='l', ylab='RR (ms)',xlab='tiempo(s)', xlim=c(0,72473),ylim=c(0,3000))
points(dato.rr$t,dato.rr$RR,pch=20)
Nmuestro = as.integer(2*max(dato.rr$t))
RRlineal = approx(dato.rr$t,dato.rr$RR,n=as.integer(68927.72799999903/0.25))
RRspline = spline(dato.rr$t,dato.rr$RR,n=as.integer(68927.72799999903/0.25))
lines (RRlineal$x, RRlineal$y, col='red')
points(RRlineal$x, RRlineal$y,pch=20,col='red')
lines (RRspline$x,RRspline$y, col='blue')
points(RRspline$x,RRspline$y,pch=20,col='green')
