dane=dane_do_zestawu_16
attach(dane)
as.data.frame(dane)
decyle=quantile(`Wielkość Spółki`,probs=seq(0.1,0.9, by=0.1) )
decyle
D4=decyle[4]
D4
D7=decyle[7]
D7

install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("writexl")
# Wczytanie pakietów
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)

min(`Wielkość Spółki`)
max(`Wielkość Spółki`)

dane <- dane %>%
  mutate(Kategoria = case_when(
    `Wielkość Spółki` <= D4 ~ "Małe",
    `Wielkość Spółki` > D4 & `Wielkość Spółki`< D7 ~ "Średnie",
    `Wielkość Spółki` >= D7 ~ "Duże"
  ))
#222222222222222222222222222222222222222
a=`Wskaźnik CZ`[dane$Kategoria=="Małe"]
a
b=`Wskaźnik CZ`[dane$Kategoria=="Średnie"]
b
c=`Wskaźnik CZ`[dane$Kategoria=="Duże"]
c

#MAŁE
mean(a)
sdm=sqrt(sum((a- mean(a))^2) / length(a))
sdm
median(a)
quantile(a, c(0.25, 0.75))
min(a)
max(a)
(length(a) / ((length(a) - 1) * (length(a) - 2))) * sum(((a - mean(a)) / sdm)^3)
(length(a) * (length(a) + 1)) / ((length(a) - 1) * (length(a) - 2) * (length(a) - 3)) * sum(((a - mean(a)) / sdm)^4) - (3 * (length(a) - 1)^2) / ((length(a) - 2) * (length(a) - 3))

#Średnie
mean(b)
sds=sqrt(sum((b- mean(b))^2) / length(b))
sds
median(b)
quantile(b, c(0.25, 0.75))
min(b)
max(b)
(length(b) / ((length(b) - 1) * (length(b) - 2))) * sum(((b - mean(b)) / sds)^3)
(length(b) * (length(b) + 1)) / ((length(b) - 1) * (length(b) - 2) * (length(b) - 3)) * sum(((b - mean(b)) / sds)^4) - (3 * (length(b) - 1)^2) / ((length(b) - 2) * (length(b) - 3))

n=length(a)
x_mean=mean(a)
diff=c-x_mean
sum_diff_4=sum(diff^4)
sum_diff_2=sum(diff^2)
kurt=(n*sum_diff_4)/(sum_diff_2^2)-3
kurt

asym=(1/length(c))*sum(((c-mean(c))/sdd)^3)
asym
#Duże
mean(c)
sdd=sqrt(sum((c- mean(c))^2) / length(c))
sdd
median(c)
quantile(c, c(0.25, 0.75))
min(c)
max(c)
(length(c) / ((length(c) - 1) * (length(c) - 2))) * sum(((c - mean(c)) / sdd)^3)
(length(c) * (length(c) + 1)) / ((length(c) - 1) * (length(c) - 2) * (length(c) - 3)) * sum(((c - mean(c)) / sdd)^4) - (3 * (length(c) - 1)^2) / ((length(c) - 2) * (length(c) - 3))

#33333333333333333333

#Małe
#dokładność
alfa=0.001
#rozstęp
ma=max(a)
mi=min(a)
R=ma-mi
#liczba klas
k=10
#długość klasy
(R+alfa/2)/k
#bardziej automatycznie
(R+alfa/2)/k*100
ceiling((R+alfa/2)/k*100)
ceiling((R+alfa/2)/k*100)/100 #100=1/alfa u nas
x=ceiling((R+alfa/2)/k/alfa)*alfa
x
#sprawdźmy czy taka długość jest wystarczająca - czy
#prawy kraniec ostatniej klasy jest większy nią wartość maksymalna
mi-alfa/2+k*x>ma

#definujemy krańce klas
krańce_klas=seq(mi-alfa/2,mi-alfa/2+k*x,by=x)
krańce_klas

#tworzymy histogram - włąsne krańce - parametr breaks
#h=hist(a,breaks=krańce_klas,
       #main = "Histogram wartości wskaźnika w grupie małych spółek",
       #xlab = "Wartości wskaźnika", ylab = "Liczba obserwacji",
       #xlim=c(0,90))
#points(wsp_x,licz_y,type="l",lwd=2,col="red")
#h$density

h1=hist(a,breaks=krańce_klas,freq=F,
       main = "Histogram wartości wskaźnika w grupie małych spółek",
       xlab = "Wartości wskaźnika", ylab = "Gęstości",
       xlim=c(0,90))
points(wsp_x,licz_y/(sum(h1$counts * diff(h1$breaks))), 
      type = "l", col = "red", lwd = 2, pch = 16)

#wygenerujmy szereg rozdzielczy
liczebności=hist(a,breaks=krańce_klas)$counts
liczebności
#lewe i prawe krańce
lewe_k=krańce_klas[1:k]
prawe_k=krańce_klas[2:(k+1)]
lewe_k
prawe_k
numer_k=1:k
#tworzymy szereg
Szereg_rozdzielczy=data.frame(numer_k,lewe_k,prawe_k,liczebności)
Szereg_rozdzielczy
write.xlsx(Szereg_rozdzielczy,'tabele.xlsx')
#Wieloboki
środki=(lewe_k+prawe_k)/2
środki
ppx=krańce_klas[1]-x[1]/2  #wsp. x pierwszego punktu
opx <- krańce_klas[length(krańce_klas)] + (krańce_klas[length(krańce_klas)] - środki[length(środki)])  # x ostatniego punktu

wsp_x1=c(ppx,środki,opx)
wsp_x1
##wielobok natężeń liczebności
licz_y1=c(0,liczebności,0)
plot(wsp_x,licz_y, type="l",lwd=2) 


#Srednie
#dokładność
alfa=0.001
#rozstęp
ma=max(b)
mi=min(b)
R=ma-mi
#liczba klas
k=9
#długość klasy
(R+alfa/2)/k
#bardziej automatycznie
(R+alfa/2)/k*100
ceiling((R+alfa/2)/k*100)
ceiling((R+alfa/2)/k*100)/100 #100=1/alfa u nas
x=ceiling((R+alfa/2)/k/alfa)*alfa
x
#sprawdźmy czy taka długość jest wystarczająca - czy
#prawy kraniec ostatniej klasy jest większy nią wartość maksymalna
mi-alfa/2+k*x>ma

#definujemy krańce klas
krańce_klas=seq(mi-alfa/2,mi-alfa/2+k*x,by=x)
krańce_klas

#tworzymy histogram - włąsne krańce - parametr breaks
#hist(b,breaks=krańce_klas,main = "Histogram wartości wskaźnika w grupie średnich spółek",xlab = "Wartości wskaźnika", ylab = "Liczba obserwacji",xlim=c(0,100),yaxt='n')
#axis(2, at = seq(0, 18, by = 2))
#points(wsp_x,licz_y,type="l",lwd=2,col="green")

h2=hist(b,breaks=krańce_klas,freq=F,main = "Histogram wartości wskaźnika w grupie średnich spółek",xlab = "Wartości wskaźnika", ylab = "Gęstości",xlim=c(0,100),ylim=c(0,0.030))
points(wsp_x,licz_y/(sum(h2$counts * diff(h2$breaks))),type="l",lwd=2,col="green")

#wygenerujmy szereg rozdzielczy
liczebności=hist(b,breaks=krańce_klas)$counts
liczebności
#lewe i prawe krańce
lewe_k=krańce_klas[1:k]
prawe_k=krańce_klas[2:(k+1)]
lewe_k
prawe_k
numer_k=1:k
#tworzymy szereg
Szereg_rozdzielczy=data.frame(numer_k,lewe_k,prawe_k,liczebności)
Szereg_rozdzielczy
write_xlsx(Szereg_rozdzielczy,'tabele.xlsx')

#Wieloboki
środki=(lewe_k+prawe_k)/2
środki
ppx=krańce_klas[1]-x[1]/2  #wsp. x pierwszego punktu
opx <- krańce_klas[length(krańce_klas)] + (krańce_klas[length(krańce_klas)] - środki[length(środki)])  # x ostatniego punktu
opx
wsp_x2=c(ppx,środki,opx)
wsp_x2
##wielobok natężeń liczebności
licz_y2=c(0,liczebności,0)
plot(wsp_x,licz_y, type="l",lwd=2) 


#Duże
#dokładność
alfa=0.001
#rozstęp
ma=max(c)
mi=min(c)
R=ma-mi
#liczba klas
k=9
#długość klasy
(R+alfa/2)/k
#bardziej automatycznie
(R+alfa/2)/k*100
ceiling((R+alfa/2)/k*100)
ceiling((R+alfa/2)/k*100)/100 #100=1/alfa u nas
x=ceiling((R+alfa/2)/k/alfa)*alfa
x
#sprawdźmy czy taka długość jest wystarczająca - czy
#prawy kraniec ostatniej klasy jest większy nią wartość maksymalna
mi-alfa/2+k*x>ma

#definujemy krańce klas
krańce_klas=seq(mi-alfa/2,mi-alfa/2+k*x,by=x)
krańce_klas

#tworzymy histogram - włąsne krańce - parametr breaks
#h=hist(c,breaks=krańce_klas,main = "Histogram wartości wzkaźnika w grupie dużych spółek",xlab = "Wartości wskaźnika", ylab = "Gęstości",xlim=c(0,100))
#points(wsp_x,licz_y,type="l",lwd=2,col="blue")

h3=hist(c,breaks=krańce_klas,freq=F,main = "Histogram wartości wzkaźnika w grupie dużych spółek",xlab = "Wartości wskaźnika", ylab = "Gęstości",xlim=c(0,100),ylim=c(0,0.020))
points(wsp_x,licz_y/(sum(h3$counts * diff(h3$breaks))),type="l",lwd=2,col="blue")


#wygenerujmy szereg rozdzielczy
liczebności=hist(c,breaks=krańce_klas)$counts
liczebności
#lewe i prawe krańce
lewe_k=krańce_klas[1:k]
prawe_k=krańce_klas[2:(k+1)]
lewe_k
prawe_k
numer_k=1:k
#tworzymy szereg
Szereg_rozdzielczy=data.frame(numer_k,lewe_k,prawe_k,liczebności)
Szereg_rozdzielczy
write_xlsx(Szereg_rozdzielczy,'tabele.xlsx')

#Wieloboki
środki=(lewe_k+prawe_k)/2
środki
ppx=krańce_klas[1]-x[1]/2  #wsp. x pierwszego punktu
opx <- krańce_klas[length(krańce_klas)] + (krańce_klas[length(krańce_klas)] - środki[length(środki)])  # x ostatniego punktu

wsp_x3=c(ppx,środki,opx)
wsp_x3
##wielobok natężeń liczebności
licz_y3=c(0,liczebności,0)
plot(wsp_x,licz_y, type="l",lwd=2) 



nat_licz=liczebności/x
nat_licz
gęst=nat_licz/length(b)
gęst

#Konstruujemy szereg dla natężeń i gęstości
nowy_szereg=data.frame(
  lewy_kr=lewe_k,prawy_kr=prawe_k,liczność=liczebności,
  natężenie_licz=nat_licz,gęstość=gęst
)
#podajmy z wartościami nat. liecznoścoi zaokrąglonymi
#do jednego, oraz gęstością do trzech miejsc po przecinku
nowy_szereg_z=data.frame(
  lewy_kr=lewe_k,prawy_kr=prawe_k,liczność=liczebności,
  natężenie_licz=round(nat_licz,1),gęstość=round(gęst,3)
)
nowy_szereg_z




#miary dla szeregu
środki_k=(lewe_k+prawe_k)/2

#średnia
śr_szeregu=1/length(c)*sum(liczebności*środki_k)
śr_szeregu
#wariancja i odchylenie standardowe
war_szeregu=1/length(c)*sum(liczebności*(środki_k-śr_szeregu)^2)
war_szeregu
odch_szeregu=sqrt(war_szeregu)
odch_szeregu

#mediana
#wyznaczamy numer odpowiedniej danej
ceiling((length(c)+1)/2)
#bardziej automatycznie - miimalny numer dla któego liczności
l=min(which(licz_sk>=ceiling((length(c)+1)/2)))
l
#wyznaczam medianę
M_szeregu=lewe_k[l]+x/liczebności[l]*(length(c)/2-licz_sk[l-1])
M_szeregu

granice=lewe_k
szerokosci=diff(granice)
liczebności
n=sum(liczebności)
n
r1=n/4
r3=3*n/4
klasa_Q1=which(cumsum(liczebności)>=r1)[1]
klasa_Q1
klasa_Q3=which(cumsum(liczebności)>=r3)[1]
y0_l=granice[klasa_Q1]
b_l=szerokosci[klasa_Q1]
n_l=liczebności[klasa_Q1]
suma_poprzednich_Q1=sum(liczebności[1:(klasa_Q1-1)])
Q1=y0_l+b_l/n_l*(r1-suma_poprzednich_Q1)
Q1

y0_p=granice[klasa_Q3]
b_p=szerokosci[klasa_Q3]
n_p=liczebności[klasa_Q3]
suma_poprzednich_Q3=sum(liczebności[1:(klasa_Q3-1)])
Q3=y0_p+b_p/n_p*(r3-suma_poprzednich_Q3)
Q3


n_i=liczebności  # liczności
y_i=środki   # średnie grupowe

# Obliczenia
n <- sum(n_i)          # suma liczności (n)
x_bar <- sum(n_i * y_i) / n  # średnia ważona (ogólna średnia)

# Trzeci moment centralny (e3)
e3 <- sum(n_i * (y_i - x_bar)^3) / n

# Czwarty moment centralny (e4)
e4 <- sum(n_i * (y_i - x_bar)^4) / n

# Odchylenie standardowe (sd)
sd <- sqrt(sum(n_i * (y_i - x_bar)^2) / n)

# Współczynnik asymetrii (gamma3)
gamma3 <- e3 / sd^3

# Kurtoza (gamma4)
gamma4 <- e4 / sd^4 - 3

density1 <- licz_y1 / sum(h1$counts * diff(h1$breaks))
density2 <- licz_y2 / sum(h2$counts * diff(h2$breaks))
density3 <- licz_y3 / sum(h3$counts * diff(h3$breaks))

# Tworzenie pustego wykresu
plot(wsp_x1, density1, type = "n", ylim = c(0, max(density1, density2, density3)),xlim=c(0,100), 
     xlab = "Wartości wskaźnika", ylab = "Wartości wieloboku gęstość", main = "Wieloboki na jednym wykresie")

# Dodanie wieloboków
lines(wsp_x1, density1, type = "l", col = "red", lwd = 2, pch = 16)  # Wielobok 1
lines(wsp_x2, density2, type = "l", col = "green", lwd = 2, pch = 16) # Wielobok 2
lines(wsp_x3, density3, type = "l", col = "blue", lwd = 2, pch = 16) # Wielobok 3

legend("topright",                      # Pozycja legendy
       legend = c("Grupa małych spółek",     # Opisy elementów legendy
                  "Grupa średnich spółek",
                  "Grupa dużych spółek"),
       col = c("red","green","blue"),         # Kolory linii w legendzie
       lty = 1,                        # Typy linii (1 = linia ciągła)
       lwd = 2,
       bty="n",
       cex=0.8)                        # Grubość linii

