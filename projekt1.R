attach(dane)
#zadanie1
table(Wykształcenie)
tabela1=data.frame(Wykształcenie=c("Podstawowe","Średnie","Wyższe","Zawodowe"),"Liczba ankietowanych"=c(44,103,51,107))
tabela1
install.packages("openxlsx")
library(openxlsx)
write.xlsx(tabela1,"tabelki.xlsx")
barplot(table(Wykształcenie),main="Wykształcenie w całej grupie ankietowanych",xlab="Rodzaj wykształcenia",ylab="Ilość ankietowanych")
prop.table(table(Wykształcenie))*100
length(Wykształcenie)
length(Wykształcenie)
tabela2=data.frame(Wykształcenie=c("Podstawowe","Średnie","Wyższe","Zawodowe"),Procentankietownych=c(14.43,33.72,16.72,35.08))
tabela2
write.xlsx(tabela2,"tabelki.xlsx")
barplot(prop.table(table(Wykształcenie))*100,main="Wykształcenie w całej grupie ankietowanych",xlab="Rodzaj wykształcenia",ylab="Procent ankietowanych")

#zadanie2 
kobiety=subset(dane,Płeć=="K")
table(kobiety$Wykształcenie)
tabela3=data.frame(Wykształcenie=c("Podstawowe","Średnie","Wyższe","Zawodowe"),Liczbaankietownych=c(32,57,19,48))
tabela3
write.xlsx(tabela3,"tabelki.xlsx")
barplot(table(kobiety$Wykształcenie),main="Wykształcenie w grupie kobiet",xlab="Rodzaj wykształcenia",ylab="Ilość ankietowanych kobiet")
prop.table(table(kobiety$Wykształcenie))*100
tabela4=data.frame(Wykształcenie=c("Podstawowe","Średnie","Wyższe","Zawodowe"),Procentankietownych=c(20.51,36.54,12.18,30.77))
tabela4
write.xlsx(tabela4,"tabelki.xlsx")
barplot(prop.table(table(kobiety$Wykształcenie))*100,main="Wykształcenie w grupie kobiet",xlab="Rodzaj wykształcenia",ylab="Procent ankietowanych kobiet")

men=subset(dane,Płeć=="M")
table(men$Wykształcenie)
tabela5=data.frame(Wykształcenie=c("Podstawowe","Średnie","Wyższe","Zawodowe"),Liczbaankietownych=c(12,46,32,59))
write.xlsx(tabela5,"tabelki.xlsx")
barplot(table(men$Wykształcenie),main="Wykształcenie w grupie mężczyzn",xlab="Rodzaj wykształcenia",ylab="Ilość ankietowanych mężczyzn")
prop.table(table(men$Wykształcenie))*100
tabela6=data.frame(Wykształcenie=c("Podstawowe","Średnie","Wyższe","Zawodowe"),Procentankietownych=c(8.05,30.87,21.48,39.60))
tabela6
write.xlsx(tabela6,"tabelki.xlsx")
barplot(prop.table(table(men$Wykształcenie))*100,main="Wykształcenie w grupie mężczyzn",xlab="Rodzaj wykształcenia",ylab="Procent ankietowanych mężczyzn")


#zadanie3
trzecie=subset(men,Zamieszkanie=="Miasto")
table(trzecie)
trzzecie1=subset(trzecie,Ubezpieczenie=="Tak")
table(trzzecie1)
tabela=data.frame(trzzecie1)
tabela
emp=ecdf(trzzecie1$Wiek)
plot(emp,main="Dystrybuanta wieku w grupie mężczyzn mieszkających w mieście, którzy się ubezpieczają",xlab="wartość x",ylab="wartość F(x)")

#zadanie4
czwarte=data.frame(Wiedza,Wykształcenie)
czwarte
table(czwarte)
write.xlsx(table(czwarte),"tabelki.xlsx")


#zadanie5
ub=Wynagrodzenie[Ubezpieczenie=="Tak"]
nub=Wynagrodzenie[Ubezpieczenie=="Nie"]
install.packages("e1071")
library(e1071)
mean(ub)
median(ub)              
sd(ub)
skewness(ub)
kurtosis(ub)

mean(nub)
median(nub)             
sd(nub)
skewness(nub)
kurtosis(nub)

#zadanie6
#miasto,duża
a=dane$Wiek[dane$Zamieszkanie=="Miasto" & dane$Wiedza=="Duża"]
mean(a)
median(a)
#miasto,średnia
b=dane$Wiek[dane$Zamieszkanie=="Miasto" & dane$Wiedza=="Średnia"]
mean(b)
median(b)
#miasto, mała
d=dane$Wiek[dane$Zamieszkanie=="Miasto" & dane$Wiedza=="Mała"]
mean(d)
median(d)
#miasto,brak
e=dane$Wiek[dane$Zamieszkanie=="Miasto" & dane$Wiedza=="Nie mam wiedzy"]
mean(e)
median(e)
#wieś,duża
f=dane$Wiek[dane$Zamieszkanie=="Wieś" & dane$Wiedza=="Duża"]
mean(f)
median(f)
#wieś,średnia
g=dane$Wiek[dane$Zamieszkanie=="Wieś" & dane$Wiedza=="Średnia"]
mean(g)
median(g)
#wieś,mała
h=dane$Wiek[dane$Zamieszkanie=="Wieś" & dane$Wiedza=="Mała"]
mean(h)
median(h)
#wieś,brak
i=dane$Wiek[dane$Zamieszkanie=="Wieś" & dane$Wiedza=="Nie mam wiedzy"]
mean(i)
median(i)
