dane=dane_spółki_16
attach(dane)
as.data.frame(dane)
wykres=ts(`Kurs akcji`, start = 1, frequency = 1)
plot(kursy, main="Kurs zamknięcia akcji spółki", xlab = "Numer sesji", 
     ylab = "Kurs akcji", type="l",lwd=1)
kursy=dane$`Kurs akcji`
n=length(dane$`Kurs akcji`)
śrch=(0.5 * kursy[1] + sum(kursy[2:(n-1)]) + 0.5 * kursy[n]) / (n - 1)
śrch
indeksy=kursy[2:n]/kursy[1:(n-1)]*100
df=data.frame(indeks=indeksy)
library(openxlsx)
write.xlsx(df,"tabele.xlsx")
tempo=prod(kursy[2:n]/kursy[1:(n-1)])^(1/(n-1))*100
tempo
plot(indeksy,type="l",ylab="Indeksy",xlab="Numer sesji",main="Indeksy łańcuchowe kursu zamknięcia")
ind=kursy[2:n]/kursy[1]*100
df1=data.frame(indeksy=ind)
write.xlsx(df1,"tabele.xlsx")
plot(ind,type="l",ylab='Indeksy',xlab="Numer sesji",main="Indeksy jednopodstawowe kursu zamknięcia")

library(zoo)
ar=rollmean(kursy, k = 81, fill = NA, align = "right")
ar=na.omit(ar)
df2=data.frame(sr=ar)
write.xlsx(df2,"tabele.xlsx")
wspx=41:134
plot(kursy,type="l",col="black",pch=16,lwd=1,
     xlab="Numer sesji",ylab="Kurs zamknięcia",main="Kurs zamknięcia akcji spółki z wygładzeniem")
points(wspx,ar,col="purple",lwd=2,type="l",pch=16)
legend("topleft",legend=c("Wygładzenie średnią 81-okresową"),
       col=c("purple"),lty=1,pch=16,cex=0.9)
