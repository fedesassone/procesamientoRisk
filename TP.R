#leer archivo

dir = ''
dato.rr = read.csv(paste( dir , '10068.MIB.csv',sep=''))
print(names(dato.rr))
# "t" "RR"
plot(dato.rr $t,dato.rr $RR,type='l', ylab='RR (ms)',xlab='tiempo (s)')
points(dato.rr $t,dato.rr $RR,pch=20)

#parte 2
tiempo_total = 71377.58399999907
constante_min_heartbeat = 0.25
delta_t = as.integer(tiempo_total/constante_min_heartbeat)
plot(dato.rr$t,dato.rr$RR,type='l', ylab='RR (ms)',xlab='tiempo(s)', xlim=c(0,72473),ylim=c(0,8000))
points(dato.rr$t,dato.rr$RR,pch=20)
Nmuestro = as.integer(2*max(dato.rr$t))
RRlineal = approx(dato.rr$t,dato.rr$RR,n=delta_t)
RRspline = spline(dato.rr$t,dato.rr$RR,n=delta_t)
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
tiempo_total = 68927.72799999903
constante_min_heartbeat = 0.25
delta_t = as.integer(tiempo_total/constante_min_heartbeat)
plot(dato.rr$t,dato.rr$RR,type='l', ylab='RR (ms)',xlab='tiempo(s)', xlim=c(0,72473),ylim=c(0,3000))
points(dato.rr$t,dato.rr$RR,pch=20)
Nmuestro = as.integer(2*max(dato.rr$t))
RRlineal = approx(dato.rr$t,dato.rr$RR,n=delta_t)
RRspline = spline(dato.rr$t,dato.rr$RR,n=delta_t)
lines (RRlineal$x, RRlineal$y, col='red')
points(RRlineal$x, RRlineal$y,pch=20,col='red')
lines (RRspline$x,RRspline$y, col='blue')
points(RRspline$x,RRspline$y,pch=20,col='green')
