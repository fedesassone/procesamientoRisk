#leer del archivo

dir = ''
dato = read.csv(paste( dir , '10068.MIB.csv',sep=""))
print (names(dato))
# [1]  "time" "RR" "beat" 
op <- par(mfrow = c(2, 2))
plot (dato$t,dato$a0,type='l ' , xlim=c(0,260),xlab='dia' , ylab='a0')
plot (dato$t,dato$a1,type='l ' , xlim=c(0,260),xlab='dia' , ylab='a1')
plot (dato$t,dato$a2,type='l ' , xlim=c(0,260),xlab='dia' , ylab='a2')
plot (dato$t,dato$rn,type='l ' , xlim=c(0,260),xlab='dia' , ylab='rn')
op <- par(mfrow = c(1, 1))
plot (dato$t,dato$a,type='l ' , xlim=c(0,260),xlab='dia' , ylab='a')




# Descomposición de una Serie Temporal con filtros de MA

outMA7 <- filter(dato$a,rep(1/7,7), circular =TRUE)
difMA7 <- dato$a- outMA7
outMA21 <- filter(dato$a,rep(1/21,21), circular =TRUE)
difMA21 <- outMA7- outMA21
op <- par(mfrow = c(2, 2))
plot (dato$t,outMA7,type='l',xlab='dia' , ylab='sal MA7')
plot (dato$t,difMA7,type='l' , xlab='dia' , ylab='dif MA7')
plot (dato$t,outMA21,type='l',xlab='dia' , ylab='sal MA21')
plot (dato$t,difMA21,type='l', xlab='dia' , ylab='dif MA21')

#otro plot:

op <- par(mfrow = c(1, 1))
plot (dato$t,outMA7,type='l',ylim=c(- 10,70),xlab='dia', ylab='sal- dif
      MA7 y MA21',col='black')
lines (dato$t,difMA7,col='blue')
lines (dato$t,outMA21,col='red')
lines (dato$t,difMA21,col='green')

#plot frecuencia

fft.a = Mod(fft(dato$a))
fft.outMA7 = Mod(fft(outMA7))
fft.difMA7 = Mod(fft(difMA7))
fft.outMA21 = Mod(fft(outMA21))
fft.difMA21 = Mod(fft(difMA21))
op <- par(mfrow = c(1, 1))
plot ( fft.a
       ,type='l ' , col='black' , ylim=c(0,800),xlim=c(0,128), lty =2)
lines ( fft.outMA7,col='black')
lines ( fft.difMA7,col='blue')
lines ( fft.outMA21,col='red')
lines ( fft.difMA21,col='green')