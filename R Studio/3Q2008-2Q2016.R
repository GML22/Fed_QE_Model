################################################################################################################
#############################                         PRACA MAGISTERSKA                    #####################
#############################       SKUTECZNOŒÆ NIEKONWENCJONALNEJ POLITYKI PIENIÊ¯NEJ     #####################
#############################        REZERWY FEDERALNEJ W CZASACH KRYZYSU FINANSOWEGO      #####################
#############################                     ANALIZA DLA LAT 2007-2015                #####################
################################################################################################################


################################################################################################################
#############################              WCZYTYWANI NIEZBÊDNYCH PAKIETÓW I DANYCH        #####################
################################################################################################################

# œcie¿ka do katalogu roboczego
setwd("C:\\Users\\GML\\Desktop\\Praca magisterska\\Modelowanie\\R Studio\\Main")

# zainstalowania niezbêdnych pakietów
install.packages("lubridate") #pakiet do dat
install.packages("lmtest")
install.packages("fBasics")
install.packages("urca")
install.packages("xts")
install.packages("MSBVAR") # for tests of Granger causality
install.packages("vars")
install.packages("xlsx", dep = T) # pakiet do importowania danych z excela
install.packages("ggplot2")
install.packages("scales")
install.packages("grid")

# oraz wczytania ich do pamieci
library(lmtest)
library(fBasics)
library(urca) # for DF tests, also Johansen test
library(xts)
library(xlsx)
library(ggplot2)
library(scales)
source("functions\\functions5.R") # testdf etc.
library(grid)
library(gridExtra)

# rm(list=ls()) # czyszczenie zmiennych
 
## wczytuje dane z pliku .xlsx
DANE <- read.xlsx("Wsad R.xlsx", sheetIndex = 1) 
DANE <- DANE[DANE$Data >= as.Date("2008-06-15"),]
 
###############################################################################################################
################################   ANALIZA W£AŒCIWOŒCI ZMIENNYCH  #############################################
###############################################################################################################

#wstêpne spojrzenie na dane
summary(DANE)

'Stworzona baza danych zawiera 33 zmienne z ró¿nych sektorów gospodarki, wybrane na podstawie analizy literatury
dotycz¹cej nikonwencjonalnej polityki polityki pieniê¿nej w USA a tak¿e w innnych krajach rozwiniêtych. Baza 
zawiera obserwacjê z lat 1Q2002-2Q2016, jednak w badaniu pos³u¿yliœmy siê tylko czêœci¹ zbioru danych przypadaj¹c¹
na lata 3Q2008-2Q2016 czyli lata najwy¿szej aktywnoœci Rezerwy Federalnej'

###############################################################################################################
############################################## CPI ############################################################

# zaczynam od wygenerowania wykresu CPI 
plot(DANE$Data, DANE$CPI,type="l",
     main="CPI w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Consumer Price Index [%]")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testami ADF i KPSS

kpss.CPI<-ur.kpss(DANE$CPI, 
                   type = c("mu")) # sta³a w równaniu testowym

summary(kpss.CPI)
# statystyka KPSS (0.2651) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic CPI.

# A zatem: CPI~I(0)

##############################################################################################################
############################################## Stopa_bez #####################################################

# zaczynam od wygenerowania wykresu Stopa_bez 
plot(DANE$Data, DANE$Stopa_bez,type="l",
     main="Stopa_bez w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Stopa bezrobocia [%]")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.Stopa_bez<-ur.kpss(DANE$Stopa_bez, 
                  type = c("mu")) # sta³a w równaniu testowym

summary(kpss.Stopa_bez)
# statystyka KPSS (1.7805) jest wiêksza od 5% wartoœci krytycznej (0.463)
# za zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Stopa_bez
plot(DANE$Data, diff.xts(DANE$Stopa_bez),type="l",
     main="Pierwsze ró¿nice wskaŸnika Stopa_bez w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice Stopa_bez [%]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Stopa_bez
kpss.Stopa_bez_d<-ur.kpss(diff.xts(DANE$Stopa_bez), 
                    type = c("mu")) 

summary(kpss.Stopa_bez_d)
# statystyka KPSS (0.9359) jest wiêksza od 5% wartoœci krytycznej (0.463)
# za zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Stopa_bez
plot(DANE$Data, diff.xts(diff.xts(DANE$Stopa_bez)),type="l",
     main="Pierwsze ró¿nice wskaŸnika Stopa_bez w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice Stopa_bez [%]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Stopa_bez
kpss.Stopa_bez_d2<-ur.kpss(diff.xts(diff.xts(DANE$Stopa_bez)), 
                          type = c("mu")) 

summary(kpss.Stopa_bez_d2)
# statystyka KPSS (0.0859) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic Stopa_bez.

# A zatem: Stopa_bez~I(2)

#############################################################################################################
############################################## PKB_realne ###################################################

# zaczynam od wykresu zmiennej PKB_realne
plot(DANE$Data, DANE$PKB_realne,type="l",
     main="PKB realne w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "PKB realne [mld $]")

# szereg wygl¹da na niestacjonarny

kpss.PKB_realne<-ur.kpss(DANE$PKB_realne, 
                  type = c("mu")) # sta³a w równaniu testowym

summary(kpss.PKB_realne)
# statystyka KPSS (1.0061) jest wiêksza od 5% wartoœci krytycznej (0.463)
# zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic  PKB realnego
plot(DANE$Data, diff.xts(DANE$PKB_realne),type="l",
     main="Pierwsze ró¿nice wskaŸnika PKB_realne w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika PKB_realne [mld $]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic PKB_realne
kpss.PKB_realne_d<-ur.kpss(diff.xts(DANE$PKB_realne), 
                   type = c("mu")) 

summary(kpss.PKB_realne_d)
# statystyka KPSS (0.0898) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic PKB_realne.

# A zatem: PKB_realne~I(1)

##############################################################################################################
############################################## SP500 #########################################################

# zaczynam od wykresu zmiennej SP500
plot(DANE$Data, DANE$SP500,type="l",
     main="S&P500 w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Indeks S&P500")

# szereg wygl¹da na niestacjonarny

kpss.SP500<-ur.kpss(DANE$SP500, 
                           type = c("mu")) # sta³a w równaniu testowym

summary(kpss.SP500)
# statystyka KPSS (2.3542) jest wiêksza od 5% wartoœci krytycznej (0.463)
# zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic  PKB realnego
plot(DANE$Data, diff.xts(DANE$SP500),type="l",
     main="Pierwsze ró¿nice wskaŸnika SP500 w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika SP500 [mld $]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic SP500
kpss.SP500_d<-ur.kpss(diff.xts(DANE$SP500), 
                           type = c("mu")) 

summary(kpss.SP500_d)
# statystyka KPSS (0.2053) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic SP500.

# A zatem: SP500~I(1)

##############################################################################################################
############################################## Yield10Gov ####################################################

# zaczynam od wykresu zmiennej Yield10Gov
plot(DANE$Data, DANE$Yield10Gov,type="l",
     main="Rentownoœæ 10-letnich obligacji skarbowych w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Rentownoœæ 10-letnich obligacji skarbowych [%]")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.Yield10Gov<-ur.kpss(DANE$Yield10Gov, 
                  type = c("mu")) # sta³a w równaniu testowym

summary(kpss.Yield10Gov)
# statystyka KPSS (1.3762) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Yield10Gov
plot(DANE$Data, diff.xts(DANE$Yield10Gov),type="l",
     main="Pierwsze ró¿nice rentownoœci 10-letnich obligacji skarbowych USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice rentownoœci 10-letnich obligacji skarbowych [%]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Yield10Gov
kpss.Yield10Gov_d<-ur.kpss(diff.xts(DANE$Yield10Gov), 
                    type = c("mu")) 

summary(kpss.Yield10Gov_d)
# statystyka KPSS (0.058) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic Yield10Gov.

# A zatem: Yield10Gov~I(1)

##############################################################################################################
############################################## FED_Securities ################################################

# zaczynam od wykresu zmiennej FED_Securities
plot(DANE$Data, DANE$FED_Securities,type="l",
     main="Rentownoœæ 10-letnich obligacji skarbowych w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Rentownoœæ 10-letnich obligacji skarbowych [%]")

DANE$FED_Securities

kpss.FED_Securities<-ur.kpss(DANE$FED_Securities, 
                     type = c("mu")) # sta³a w równaniu testowym

summary(kpss.FED_Securities)
# statystyka KPSS (2.3543) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic FED_Securities
plot(DANE$Data, diff.xts(DANE$FED_Securities),type="l",
     main="Pierwsze ró¿nice rentownoœci 10-letnich obligacji skarbowych USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice rentownoœci 10-letnich obligacji skarbowych [%]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic FED_Securities
kpss.FED_Securities_d<-ur.kpss(diff.xts(DANE$FED_Securities), 
                           type = c("mu")) 

summary(kpss.FED_Securities_d)
# statystyka KPSS (0.3538) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic FED_Securities.

# A zatem: FED_Securities~I(1)

##########################################################################################################
############################################## Yield1YGov ################################################

# zaczynam od wykresu zmiennej Yield1YGov
plot(DANE$Data, DANE$Yield1YGov,type="l",
     main="WskaŸnik Yield1YGov w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik Yield1YGov [%]")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.Yield1YGov<-ur.kpss(DANE$Yield1YGov, 
                         type = c("mu")) # sta³a w równaniu testowym

summary(kpss.Yield1YGov)
# statystyka KPSS (0.7421) jest wiêksza od 5% wartoœci krytycznej (0.463)
# za zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Yield1YGov
plot(DANE$Data, diff.xts(DANE$Yield1YGov),type="l",
     main="Pierwsze ró¿nice wskaŸnika Yield1YGov w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika Yield1YGov [%]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Yield1YGov
kpss.Yield1YGov_d<-ur.kpss(diff.xts(DANE$Yield1YGov), 
                           type = c("mu")) 

summary(kpss.Yield1YGov_d)
# statystyka KPSS (0.6648) jest wiêksza od 5% wartoœci krytycznej (0.463)
# za zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Yield1YGov
plot(DANE$Data, diff.xts(diff.xts(DANE$Yield1YGov)),type="l",
     main="Pierwsze ró¿nice wskaŸnika Yield1YGov w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika Yield1YGov [%]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Yield1YGov
kpss.Yield1YGov_d2<-ur.kpss(diff.xts(diff.xts(DANE$Yield1YGov)), 
                           type = c("mu")) 

summary(kpss.Yield1YGov_d2)
# statystyka KPSS (0.0282) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic Yield1YGov.

# A zatem: Yield1YGov~I(2)

##############################################################################################################
############################################## EURUSD ########################################################

# zaczynam od wykresu zmiennej EURUSD
plot(DANE$Data, DANE$EURUSD,type="l",
     main="WskaŸnik EURUSD w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik EURUSD")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.EURUSD<-ur.kpss(DANE$EURUSD, 
                   type = c("mu")) # sta³a w równaniu testowym

summary(kpss.EURUSD)

# statystyka KPSS (1.4016) jest wiêksza od 5% wartoœci krytycznej (0.463)
# za zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic EURUSD
plot(DANE$Data, diff.xts(DANE$EURUSD),type="l",
     main="Pierwsze ró¿nice wskaŸnika EURUSD w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika EURUSD")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic EURUSD
kpss.EURUSD_d<-ur.kpss(diff.xts(DANE$EURUSD), 
                     type = c("mu")) 

summary(kpss.EURUSD_d)

# statystyka KPSS (0.0683) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic EURUSD.

# A zatem: EURUSD~I(1)

#############################################################################################################
############################################## UnempDur #####################################################

# zaczynam od wykresu zmiennej UnempDur
plot(DANE$Data, DANE$UnempDur,type="l",
     main="WskaŸnik UnempDur w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik UnempDur")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.UnempDur<-ur.kpss(DANE$UnempDur, 
                  type = c("mu")) # sta³a w równaniu testowym

summary(kpss.UnempDur)
# statystyka KPSS (0.8413) jest wiêksza od 5% wartoœci krytycznej (0.463)
# za zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic UnempDur
plot(DANE$Data, diff.xts(DANE$UnempDur),type="l",
     main="Pierwsze ró¿nice wskaŸnika UnempDur w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika UnempDur")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic UnempDur
kpss.UnempDur_d<-ur.kpss(diff.xts(DANE$UnempDur), 
                    type = c("mu")) 

summary(kpss.UnempDur_d)
# statystyka KPSS (0.3679) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic UnempDur.

# A zatem: UnempDur~I(1)

#############################################################################################################
############################################## IPP ##########################################################

# zaczynam od wykresu zmiennej IPP
plot(DANE$Data, DANE$IPP,type="l",
     main="Index Produkcji przemys³owej w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Index Produkcji Przemys³owej [%]")

# wykres nie wskazuje na stacjonarnoœæ lub jej brak 

kpss.IPP<-ur.kpss(DANE$IPP, 
                  type = c("mu")) # sta³a w równaniu testowym

summary(kpss.IPP)
# statystyka KPSS (0.6248) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic IPP
plot(DANE$Data, diff.xts(DANE$IPP),type="l",
     main="Pierwsze ró¿nice rentownoœci 10-letnich obligacji skarbowych USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice rentownoœci 10-letnich obligacji skarbowych [%]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic IPP
kpss.IPP_d<-ur.kpss(diff.xts(DANE$IPP), 
                    type = c("mu")) 

summary(kpss.IPP_d)
# statystyka KPSS (0.1146) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic IPP.

# A zatem: IPP~I(1)
##############################################################################################################
############################################## House_Price ###################################################

# zaczynam od wykresu zmiennej House_Price
plot(DANE$Data, DANE$House_Price,type="l",
     main="WskaŸnik House_Price w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik House_Price [%]")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.House_Price<-ur.kpss(DANE$House_Price, 
                          type = c("mu")) # sta³a w równaniu testowym

summary(kpss.House_Price)
# statystyka KPSS (2.0957) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic House_Price
plot(DANE$Data, diff.xts(DANE$House_Price),type="l",
     main="Pierwsze ró¿nice wskaŸnika House_Price w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika House_Price [%]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic House_Price
kpss.House_Price_d<-ur.kpss(diff.xts(DANE$House_Price), 
                            type = c("mu")) 

summary(kpss.House_Price_d)
# statystyka KPSS (0.2157) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci drugich ró¿nic House_Price.

# A zatem: House_Price~I(1)

############################################################################################################
############################################## Gold_Price ##################################################

# zaczynam od wykresu zmiennej Gold_Price
plot(DANE$Data, DANE$Gold_Price,type="l",
     main="Cena z³ota w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Cena z³ota [$]")

# szereg wygl¹da na niestacjonarny

kpss.Gold_Price<-ur.kpss(DANE$Gold_Price, 
                         type = c("mu")) # sta³a w równaniu testowym

summary(kpss.Gold_Price)
# statystyka KPSS (0.6622) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic  zmiennej Gold_Price
plot(DANE$Data, diff.xts(DANE$Gold_Price),type="l",
     main="Pierwsze ró¿nice ceny z³ota latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice ceny z³ota")

# cie¿ko rozstrzygn¹æ o stacjonarnoœci szerego na podstawie wykresu -> trzeba przeprowadziæ test

kpss.Gold_Price_d<-ur.kpss(diff.xts(DANE$Gold_Price), 
                           type = c("mu")) 

summary(kpss.Gold_Price_d)
# statystyka KPSS (0.2995) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci drugich ró¿nic  zmiennej Gold_Price.

# A zatem: Gold_Price~I(1)

###########################################################################################################
############################################## CorpAAAYields ##############################################

# zaczynam od wykresu zmiennej CorpAAAYields
plot(DANE$Data, DANE$CorpAAAYields,type="l",
     main="WskaŸnik CorpAAAYields w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik CorpAAAYields")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.CorpAAAYields<-ur.kpss(DANE$CorpAAAYields, 
                            type = c("mu")) # sta³a w równaniu testowym

summary(kpss.CorpAAAYields)

# statystyka KPSS (1.1971) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic CorpAAAYields
plot(DANE$Data, diff.xts(DANE$CorpAAAYields),type="l",
     main="Pierwsze ró¿nice wskaŸnika CorpAAAYields w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika CorpAAAYields")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic CorpAAAYields
kpss.CorpAAAYields_d<-ur.kpss(diff.xts(DANE$CorpAAAYields), 
                              type = c("mu")) 

summary(kpss.CorpAAAYields_d)

# statystyka KPSS (0.2043) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic CorpAAAYields.

# A zatem: CorpAAAYields~I(1)

#############################################################################################################
############################################## VIX ##########################################################

# zaczynam od wykresu zmiennej VIX
plot(DANE$Data, DANE$VIX,type="l",
     main="WskaŸnik VIX w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik VIX")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.VIX<-ur.kpss(DANE$VIX, 
                  type = c("mu")) # sta³a w równaniu testowym

summary(kpss.VIX)
# statystyka KPSS (1.2426) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic VIX
plot(DANE$Data, diff.xts(DANE$VIX),type="l",
     main="Pierwsze ró¿nice wskaŸnika VIX w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika VIX")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic VIX
kpss.VIX_d<-ur.kpss(diff.xts(DANE$VIX), 
                    type = c("mu")) 

summary(kpss.VIX_d)

# statystyka KPSS (0.0351) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic VIX.

# A zatem: VIX~I(1)

#############################################################################################################
############################################## Swap10Year ###################################################

# zaczynam od wykresu zmiennej Swap10Year
plot(DANE$Data, DANE$Swap10Year,type="l",
     main="WskaŸnik Swap10Year w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik Swap10Year")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.Swap10Year<-ur.kpss(DANE$Swap10Year, 
                         type = c("mu")) # sta³a w równaniu testowym

summary(kpss.Swap10Year)
# statystyka KPSS (1.4871) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Swap10Year
plot(DANE$Data, diff.xts(DANE$Swap10Year),type="l",
     main="Pierwsze ró¿nice wskaŸnika Swap10Year w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika Swap10Year")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Swap10Year
kpss.Swap10Year_d<-ur.kpss(diff.xts(DANE$Swap10Year), 
                           type = c("mu")) 

summary(kpss.Swap10Year_d)
# statystyka KPSS (0.0835) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic Swap10Year.

# A zatem: Swap10Year~I(1)

##############################################################################################################
############################################## EFFR ##########################################################

# zaczynam od wykresu zmiennej EFFR
plot(DANE$Data, DANE$EFFR,type="l",
     main="WskaŸnik EFFR w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik EFFR")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.EFFR<-ur.kpss(DANE$EFFR, 
                   type = c("mu")) # sta³a w równaniu testowym

summary(kpss.EFFR)
# statystyka KPSS (0.4616) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci zmiennej EFFR.

# A zatem: EFFR~I(0)

##############################################################################################################
############################################## Kred_bank #####################################################

# zaczynam od wykresu zmiennej Kred_bank
plot(DANE$Data, DANE$Kred_bank,type="l",
     main="WskaŸnik Kred_bank w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik Kred_bank")

# szereg wygl¹da na niestacjonarny

# Warto zlogarytmowaæ zmienn¹ Kred_bank
plot(DANE$Data, log(DANE$Kred_bank),type="l",
     main="WskaŸnik Kred_bank w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik Kred_bank")

kpss.Kred_bank_log<-ur.kpss(log(DANE$Kred_bank), 
                            type = c("mu")) # sta³a w równaniu testowym

summary(kpss.Kred_bank_log)
# statystyka KPSS (2.2556) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Kred_bank
plot(DANE$Data, diff.xts(log(DANE$Kred_bank)),type="l",
     main="Pierwsze ró¿nice wskaŸnika Kred_bank w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika Kred_bank")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Kred_bank
kpss.Kred_bank_log_d<-ur.kpss(diff.xts(log(DANE$Kred_bank)), 
                              type = c("mu")) 

summary(kpss.Kred_bank_log_d)
# statystyka KPSS (0.6045) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres drugich ró¿nic Kred_bank
plot(DANE$Data, diff.xts(diff.xts(log(DANE$Kred_bank))),type="l",
     main="Pierwsze ró¿nice wskaŸnika Kred_bank w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice Kred_bank [%]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Kred_bank
kpss.Kred_bank_log_d2<-ur.kpss(diff.xts(diff.xts(log(DANE$Kred_bank))), 
                               type = c("mu")) 

summary(kpss.Kred_bank_log_d2)
# statystyka KPSS (0.0233) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci drugich ró¿nic Kred_bank.

# A zatem: log(Kred_bank)~I(2)

############################################################################################################
############################################## InflExp #####################################################

# zaczynam od wykresu zmiennej InflExp
plot(DANE$Data, DANE$InflExp,type="l",
     main="WskaŸnik InflExp w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik InflExp")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.InflExp<-ur.kpss(DANE$InflExp, 
                      type = c("mu")) # sta³a w równaniu testowym

summary(kpss.InflExp)
# statystyka KPSS (0.2316) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic InflExp.

# A zatem: InflExp~I(0)

#############################################################################################################
############################################## Swap30Year #####################################################

# zaczynam od wykresu zmiennej Swap30Year
plot(DANE$Data, DANE$Swap30Year,type="l",
     main="WskaŸnik Swap30Year w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik Swap30Year")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.Swap30Year<-ur.kpss(DANE$Swap30Year, 
                         type = c("mu")) # sta³a w równaniu testowym

summary(kpss.Swap30Year)
# statystyka KPSS (1.3701) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Swap30Year
plot(DANE$Data, diff.xts(DANE$Swap30Year),type="l",
     main="Pierwsze ró¿nice wskaŸnika Swap30Year w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika Swap30Year")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Swap30Year
kpss.Swap30Year_d<-ur.kpss(diff.xts(DANE$Swap30Year), 
                           type = c("mu")) 

summary(kpss.Swap30Year_d)
# statystyka KPSS (0.0554) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic Swap30Year.

# A zatem: Swap30Year~I(1)

##############################################################################################################
############################################## Swap1Year #####################################################

# zaczynam od wykresu zmiennej Swap1Year
plot(DANE$Data, DANE$Swap1Year,type="l",
     main="WskaŸnik Swap1Year w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik Swap1Year")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.Swap1Year<-ur.kpss(DANE$Swap1Year, 
                        type = c("mu")) # sta³a w równaniu testowym

summary(kpss.Swap1Year)
# statystyka KPSS (0.8896) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Swap1Year
plot(DANE$Data, diff.xts(DANE$Swap1Year),type="l",
     main="Pierwsze ró¿nice wskaŸnika Swap1Year w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika Swap1Year")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Swap1Year
kpss.Swap1Year_d<-ur.kpss(diff.xts(DANE$Swap1Year), 
                          type = c("mu")) 

summary(kpss.Swap1Year_d)
# statystyka KPSS (0.748) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres drugich ró¿nic Swap1Year
plot(DANE$Data, diff.xts(diff.xts(log(DANE$Swap1Year))),type="l",
     main="Pierwsze ró¿nice wskaŸnika Swap1Year w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice Swap1Year [%]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Swap1Year
kpss.Swap1Year_log_d2<-ur.kpss(diff.xts(diff.xts(DANE$Swap1Year)), 
                               type = c("mu")) 

summary(kpss.Swap1Year_log_d2)
# statystyka KPSS (0.0247) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci drugich ró¿nic Swap1Year.

# A zatem: Swap1Year~I(2)

#############################################################################################################
############################################## CorpBBBYields ################################################

# zaczynam od wykresu zmiennej CorpBBBYields
plot(DANE$Data, DANE$CorpBBBYields,type="l",
     main="WskaŸnik CorpBBBYields w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik CorpBBBYields")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.CorpBBBYields<-ur.kpss(DANE$CorpBBBYields, 
                            type = c("mu")) # sta³a w równaniu testowym

summary(kpss.CorpBBBYields)
# statystyka KPSS (1.5041) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic CorpBBBYields
plot(DANE$Data, diff.xts(DANE$CorpBBBYields),type="l",
     main="Pierwsze ró¿nice wskaŸnika CorpBBBYields w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika CorpBBBYields")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic CorpBBBYields
kpss.CorpBBBYields_d<-ur.kpss(diff.xts(DANE$CorpBBBYields), 
                              type = c("mu")) 

summary(kpss.CorpBBBYields_d)
# statystyka KPSS (0.0912) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic CorpBBBYields.

# A zatem: CorpBBBYields~I(1)

############################################################################################################
############################################## CorpCCCYields ###############################################

# zaczynam od wykresu zmiennej CorpCCCYields
plot(DANE$Data, DANE$CorpCCCYields,type="l",
     main="WskaŸnik CorpCCCYields w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik CorpCCCYields")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.CorpCCCYields<-ur.kpss(DANE$CorpCCCYields, 
                            type = c("mu")) # sta³a w równaniu testowym

summary(kpss.CorpCCCYields)
# statystyka KPSS (0.7418) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic CorpCCCYields
plot(DANE$Data, diff.xts(DANE$CorpCCCYields),type="l",
     main="Pierwsze ró¿nice wskaŸnika CorpCCCYields w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika CorpCCCYields")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic CorpCCCYields
kpss.CorpCCCYields_d<-ur.kpss(diff.xts(DANE$CorpCCCYields), 
                              type = c("mu")) 

summary(kpss.CorpCCCYields_d)
# statystyka KPSS (0.0646) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic CorpCCCYields.

# A zatem: CorpCCCYields~I(1)

#############################################################################################################

############################################## PPI #####################################################

# zaczynam od wykresu zmiennej PPI
plot(DANE$Data, DANE$PPI,type="l",
     main="WskaŸnik PPI w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik PPI")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.PPI<-ur.kpss(DANE$PPI, 
                  type = c("mu")) # sta³a w równaniu testowym

summary(kpss.PPI)
# statystyka KPSS (0.5941) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci zmiennej PPI.

# A zatem: PPI~I(0)

############################################## Zatrudnienie #####################################################

# zaczynam od wykresu zmiennej Zatrudnienie
plot(DANE$Data, DANE$Zatrudnienie,type="l",
     main="WskaŸnik Zatrudnienie w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik Zatrudnienie")

# warto zlogarytmowaæ zmienn¹ Zatrudnienie
plot(DANE$Data, log(DANE$Zatrudnienie),type="l",
     main="WskaŸnik Zatrudnienie w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik Zatrudnienie")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.Zatrudnienie_log<-ur.kpss(log(DANE$Zatrudnienie), 
                               type = c("mu")) # sta³a w równaniu testowym

summary(kpss.Zatrudnienie_log)
# statystyka KPSS (2.0013) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Zatrudnienie
plot(DANE$Data, diff.xts(log(DANE$Zatrudnienie)),type="l",
     main="Pierwsze ró¿nice wskaŸnika Zatrudnienie w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika Zatrudnienie")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Zatrudnienie
kpss.Zatrudnienie_log_d<-ur.kpss(diff.xts(log(DANE$Zatrudnienie)), 
                                 type = c("mu")) 

summary(kpss.Zatrudnienie_log_d)
# statystyka KPSS (1.3149) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres drugich ró¿nic Zatrudnienie
plot(DANE$Data, diff.xts(diff.xts(log(DANE$Zatrudnienie))),type="l",
     main="Pierwsze ró¿nice wskaŸnika Zatrudnienie w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice Zatrudnienie [%]")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Zatrudnienie
kpss.Zatrudnienie_d2<-ur.kpss(diff.xts(diff.xts(log(DANE$Zatrudnienie))), 
                              type = c("mu")) 

summary(kpss.Zatrudnienie_d2)
# statystyka KPSS (0.0518) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci drugich ró¿nic Zatrudnienie.

# A zatem: log(Zatrudnienie)~I(2)

############################################## Baza_mon #####################################################

# zaczynam od wykresu zmiennej Baza_mon
plot(DANE$Data, DANE$Baza_mon,type="l",
     main="WskaŸnik Baza_mon w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik Baza_mon")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.Baza_mon<-ur.kpss(DANE$Baza_mon, 
                           type = c("mu")) # sta³a w równaniu testowym

summary(kpss.Baza_mon)
# statystyka KPSS (2.3989) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Baza_mon
plot(DANE$Data, diff.xts(DANE$Baza_mon),type="l",
     main="Pierwsze ró¿nice wskaŸnika Baza_mon w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika Baza_mon")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Baza_mon
kpss.Baza_mon_d<-ur.kpss(diff.xts(DANE$Baza_mon), 
                             type = c("mu")) 

summary(kpss.Baza_mon_d)
# statystyka KPSS (0.3011) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic Baza_mon.

# A zatem: Baza_mon~I(1)

############################################## Pers_cons #####################################################

# zaczynam od wykresu zmiennej Pers_cons
plot(DANE$Data, DANE$Pers_cons,type="l",
     main="WskaŸnik Pers_cons w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik Pers_cons")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.Pers_cons<-ur.kpss(DANE$Pers_cons, 
                        type = c("mu")) # sta³a w równaniu testowym

summary(kpss.Pers_cons)
# statystyka KPSS (2.4386) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Pers_cons
plot(DANE$Data, diff.xts(DANE$Pers_cons),type="l",
     main="Pierwsze ró¿nice wskaŸnika Pers_cons w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika Pers_cons")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Pers_cons
kpss.Pers_cons_d<-ur.kpss(diff.xts(DANE$Pers_cons), 
                          type = c("mu")) 

summary(kpss.Pers_cons_d)
# statystyka KPSS (0.1719) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic Pers_cons.

# A zatem: Pers_cons~I(1)

############################################## TED_spread #####################################################

# zaczynam od wykresu zmiennej TED_spread
plot(DANE$Data, DANE$TED_spread,type="l",
     main="WskaŸnik TED_spread w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik TED_spread")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.TED_spread<-ur.kpss(DANE$TED_spread, 
                         type = c("mu")) # sta³a w równaniu testowym

summary(kpss.TED_spread)
# statystyka KPSS (0.6788) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic TED_spread
plot(DANE$Data, diff.xts(DANE$TED_spread),type="l",
     main="Pierwsze ró¿nice wskaŸnika TED_spread w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika TED_spread")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic TED_spread
kpss.TED_spread_d<-ur.kpss(diff.xts(DANE$TED_spread), 
                           type = c("mu")) 

summary(kpss.TED_spread_d)
# statystyka KPSS (0.0731) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic TED_spread.

# A zatem: TED_spread~I(1)

############################################## Spread2Y10Y #####################################################

# zaczynam od wykresu zmiennej Spread2Y10Y
plot(DANE$Data, DANE$Spread2Y10Y,type="l",
     main="WskaŸnik Spread2Y10Y w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik Spread2Y10Y")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.Spread2Y10Y<-ur.kpss(DANE$Spread2Y10Y, 
                          type = c("mu")) # sta³a w równaniu testowym

summary(kpss.Spread2Y10Y)
# statystyka KPSS (0.9253) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic Spread2Y10Y
plot(DANE$Data, diff.xts(DANE$Spread2Y10Y),type="l",
     main="Pierwsze ró¿nice wskaŸnika Spread2Y10Y w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika Spread2Y10Y")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic Spread2Y10Y
kpss.Spread2Y10Y_d<-ur.kpss(diff.xts(DANE$Spread2Y10Y), 
                            type = c("mu")) 

summary(kpss.Spread2Y10Y_d)
# statystyka KPSS (0.2444) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic Spread2Y10Y.

# A zatem: Spread2Y10Y~I(1)

############################################## GBPUSD #####################################################

# zaczynam od wykresu zmiennej GBPUSD
plot(DANE$Data, DANE$GBPUSD,type="l",
     main="WskaŸnik GBPUSD w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik GBPUSD")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.GBPUSD<-ur.kpss(DANE$GBPUSD, 
                     type = c("mu")) # sta³a w równaniu testowym

summary(kpss.GBPUSD)
# statystyka KPSS (0.2947) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic GBPUSD.

# A zatem: GBPUSD~I(0)

############################################## USDJPY #####################################################

# zaczynam od wykresu zmiennej USDJPY
plot(DANE$Data, DANE$USDJPY,type="l",
     main="WskaŸnik USDJPY w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik USDJPY")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.USDJPY<-ur.kpss(DANE$USDJPY, 
                     type = c("mu")) # sta³a w równaniu testowym

summary(kpss.USDJPY)
# statystyka KPSS (1.2952) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic USDJPY
plot(DANE$Data, diff.xts(DANE$USDJPY),type="l",
     main="Pierwsze ró¿nice wskaŸnika USDJPY w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika USDJPY")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic USDJPY
kpss.USDJPY_d<-ur.kpss(diff.xts(DANE$USDJPY), 
                       type = c("mu")) 

summary(kpss.USDJPY_d)
# statystyka KPSS (0.3091) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic USDJPY.

# A zatem: USDJPY~I(1)

#############################################################################################################
############################################## USDCNY #####################################################

# zaczynam od wykresu zmiennej USDCNY
plot(DANE$Data, DANE$USDCNY,type="l",
     main="WskaŸnik USDCNY w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "WskaŸnik USDCNY")

# szereg wygl¹da na niestacjonarny, warto to potwiedzic testem KPSS

kpss.USDCNY<-ur.kpss(DANE$USDCNY, 
                     type = c("mu")) # sta³a w równaniu testowym

summary(kpss.USDCNY)
# statystyka KPSS (1.7815) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic USDCNY
plot(DANE$Data, diff.xts(DANE$USDCNY),type="l",
     main="Pierwsze ró¿nice wskaŸnika USDCNY w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika USDCNY")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic USDCNY
kpss.USDCNY_d<-ur.kpss(diff.xts(DANE$USDCNY), 
                       type = c("mu")) 

summary(kpss.USDCNY_d)
# statystyka KPSS (0.6513) jest wiêksza od 5% wartoœci krytycznej (0.463)
# a zatem odrzucamy H0 o stacjonarnosci 

# wykres pierwszych ró¿nic USDCNY
plot(DANE$Data, diff.xts(diff.xts(DANE$USDCNY)),type="l",
     main="Pierwsze ró¿nice wskaŸnika USDCNY w USA w latach 3Q2008-2Q2016", 
     xlab = "Rok", 
     ylab = "Pierwsze ró¿nice wskaŸnika USDCNY")

# szereg pierwszych ró¿nic wygl¹da na stacjonarny, ale warto potwierdziæ operacjê restem KPSS

# powtarzm procedurê dla pierwszych ró¿nic USDCNY
kpss.USDCNY_d2<-ur.kpss(diff.xts(diff.xts(DANE$USDCNY)), 
                       type = c("mu")) 

summary(kpss.USDCNY_d2)
# statystyka KPSS (0.499) jest mniejsza od 5% wartoœci krytycznej (0.463)
# a zatem nie mo¿emy odrzuciæ H0 o stacjonarnoœci pierwszych ró¿nic USDCNY.

# A zatem: USDCNY~I(2)

#############################################################################################################
###################################   WYBÓR ZMIENNYCH DO MODELU   ###########################################

# Otrzymaliœmy zmienne zintegrowane w nastêpuj¹cych stopniach:
# - CPI~I(0),
# - Stopa_bez~I(2),
# - PKB_realne~I(1),
# - SP500~I(1),
# - Yield10Gov~I(1),
# - Yield1YGov~I(2),
# - FED_Securities~I(1)
# - UnempDur~I(1),
# - EURUSD~I(1),
# - IPP~I(1),
# - House_Price~I(1),
# - Gold_Price~I(1),
# - CorpAAAYields~I(1),
# - VIX~I(1),
# - Swap10Year~I(1),
# - EFFR~I(0),
# - log(Kred_bank)~I(2),
# - InflExp~I(0),
# - Swap10Year~I(1),
# - Swap30Year~I(1),
# - Swap1Year~I(2),
# - CorpBBBYields~I(1),
# - CorpCCCYields~I(1),
# - PPI~I(0),
# - log(Zatrudnienie)~I(2),
# - Baza_mon~I(1),
# - Pers_cons~I(1),
# - TED_spread~I(1),
# - Spread2Y10Y~I(1),
# - GBPUSD~I(0),
# - USDJPY~I(1),
# - USDCNY~I(2).

'Po wielu analizach zarówno pod k¹tem literatury, jak i konstrukcji modelu, jego jakoœci dopasowania do danych,
stabilnoœci i z³o¿onoœci, zdecydowa³em sie do finalnego modelu wykorzystaæ nastêpuj¹ce 9 zmiennych, gwarantuj¹cych
najlepsz¹ jakoœæ finalnego modelu:'
# - CPI~I(0),
# - Stopa_bez~I(2),
# - PKB_realne~I(1),
# - SP500~I(1),
# - Yield10Gov~I(1),
# - Yield1YGov~I(2),
# - FED_Securities~I(1)
# - UnempDur~I(1),
# - EURUSD~I(1),

'W zwi¹zku z tym, i¿ wœród tak dobranych zmiennych mamy jedn¹ zmienn¹ zintegrowan¹ w stopniu zerowym i dwie
w stopniu drugim, nie mo¿emy skorzystaæ z modelu wektorowej korekty b³êdem. Nie powinniœmy te¿ interpetowaæ
oszacowañ bet w modelu VAR, gdy¿ brak stacjonarnoœci zmiennych moze prowadziæ do problemu regresji pozornej. 
Ró¿nicowanie zmiennych do stacjonarnoœci (szczególnie tych zintegrowanyh w I(2)) po to by móc korzystaæ z 
oszacowañ modelu doprowadzi³oby do utraty informacji o zale¿noœciach d³ogoterminowych pomiêdzy zmiennymi, 
st¹d te¿ do modelu VAR wykorzystam zmienne niezró¿nicowane, nie bêdê przy tym interpretowa³ oszacowañ bet,
a skupiê siê na d³ugoterminowych zale¿nosciach pomiêdzy zmiennymi, czyli wykresach funkcji odpowiedzi na 
impuls, b³êdów prognoz oraz dekompozji wariancji b³êdów prognoz'

##############################################################################################################
#########################################       MODEL VAR     ################################################
##############################################################################################################

# wczytanie biblioteki potrzebnej do oszacowania modelu VAR
library(vars)

DANE_Fin <- DANE[,-c(8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 21, 22, 25, 26, 27, 28, 29, 30, 31, 32, 33)]

VARselect(DANE_Fin[,2:10], lag.max = 6, type = "both") # maksymalna liczba opóŸnieñ

# im mniejsza wartoœæ kryterium, tym lepszy model, kryteria wskazuj¹ nastêpuj¹ce liczby opóŸnieñ
# AIC(n) - 6,  HQ(n) - 2,  SC(n) - 1, FPE(n) - 3. Wybiorê model z 3 opóŸnieniami, bo najlepiej wychodzi w testch 
# jakoœci modeli

#Deklaracja modelu VAR z 3 opóŸnieniami
USA_VAR <- VAR(DANE_Fin[,2:10], p = 3, type = "both")

#wykresy jakoœci dopasowania modelu, reszt i ACF i PACF
plot (USA_VAR)

summary(USA_VAR)

# sprawdzanie pierwiastków jednostkowych modelu, czy nie przekraczaja 1, co zapewnia stabilnoœæ modelu
roots<-roots(USA_VAR)
max(roots)

# Zarówno wykresy jak i test pierwiastków pokazuj¹, i¿ regresja jest dobrze dopasowana dla wszystkich 10 zmiennych,
# modu³y wartoœci w³asnych towarzysz¹cych macierzy nie przekraczaj¹ 1 co oznacza stabilnSoœæ modelu

#################################
# testy diagnostyczne modelu VAR
#################################

# test na autokorelacjê reszt

ser11 <- serial.test(USA_VAR)
ser11$serial

# p-value = 0.1482 - nie ma podstaw do odrzucenia H0 o braku autokorelacji wœród reszt modelu

# Testy na normalnoœæ reszt
norm1 <- normality.test(USA_VAR)
norm1$jb.mul

# p-value we wszystkich 3 Testach jest wy¿sze od 5% wartoœci krytycznej co oznacza, ¿e reszty maj¹ rozk³ad normalny

# Test heteroskedastycznoœæ
arch1 <- arch.test(USA_VAR, lags.multi = 3, multivariate.only = TRUE)
arch1$arch.mul

'p-value = 1, wskazuje na heteroskedastycznoœæ wœród reszt modelu, co czêsto siê zdarza w przypadku danych
makroekonomicznych szczególnie w czasach tak du¿ych zawirowañ finansowych. Heteroskedastycznoœæ mo¿e pogorszyæ
poprawnoœæ oszacowañ bet w modelu - jednak ze wzglêdu na brak stacjonarnoœci zmiennych w modelu i tak nie 
jesteœmy zainteresowani oszacowaniami bet tylko d³ugoterminowymi zale¿noœciami pomiêdzy zmiennymi'

#wykres zmiennej FED_Securities
FED_BS2 <- ggplot(DANE, aes(x=DANE$Data, y=DANE$FED_Securities)) + scale_y_continuous(expand=c(0.01,0.01), labels = scales::comma, oob = rescale_none) + 
  geom_line(size = 1.5, color = "mediumblue") +  geom_area(fill="lightblue", alpha = 0.6) + 
  labs(x = '', y ='Wartoœæ papierów wartoœciowych w bilansie Rezerwy Federalnej [miliony dolarów]') + 
  theme_light() +  scale_x_date(breaks = date_breaks("year"), labels = date_format("%Y"), expand=c(0,0)) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)))

FED_BS2

ggsave("FEDSC.pdf", width = 14, height = 8)

'W zwi¹zku z tym, ¿e chcia³bym sprawdziæ wp³yw zmiennej FED_securities czyli wartoœci zakupionych przez 
Rezerwê Federaln¹ papierów wartoœciowych, na pozosta³e zmiennê muszê spawdziæ czy zmienna ta jest przyczyn¹
w sensie Grangera pozosta³ych zmiennych w tym celu pos³uzê sie funkcja causality:'

causality(USA_VAR, cause = "FED_Securities")

'Granger: p-value = 3.221e-05, Instantenous: p-value = 0.007985. Wartoœci p-value w obu testach s¹ ma³e 
(mniejsze ni¿ 5%), wiêc mo¿emy odrzuciæ H0 o braku przyczynowoœci - FED_Securities jest przyczyn¹ w sensie Grangera 
pozosta³ych zmiennych - jest podstawa do tego by generowaæ funkcjê odpowiedzi na szok zmiennej FED_securities'

#################################### Impulse Response Function ####################################################
# CPI #
pre_IRF = irf(USA_VAR, impulse = "FED_Securities", response = c("CPI"),n.ahead = 12, cumulative = TRUE,
              seed = 123456)

CPI_IRF <- data.frame(t = seq(from = 0, to = 12, by = 1),
                      Val_IRF = pre_IRF$irf$FED_Securities,
                      Upp_IRF = pre_IRF$Upper$FED_Securities,
                      Low_IRF = pre_IRF$Lower$FED_Securities)

names(CPI_IRF) <- c("t", "Val_IRF", "Upp_IRF", "Low_IRF")

CPI_plot <- ggplot(CPI_IRF, aes(x = t, y = Val_IRF)) + geom_line(colour = "red", size = 2) + ggtitle("CPI") + 
                   scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + 
                   labs(x = 'Okres odpowiedzi na impuls [w miesi¹cach]', y = 'Punkty procentowe') + 
                   theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
                   theme(axis.title.x = element_text(margin = margin(10,0,0,0))) +
                   geom_line(aes(x = t, y = Upp_IRF), colour = "darkblue", linetype = 2, size = 1) +
                   geom_line(aes(x = t, y = Low_IRF), colour = "darkblue", linetype = 2, size = 1) +
                   geom_ribbon(aes(ymin = Low_IRF, ymax = Upp_IRF),
                   fill = "lightblue", alpha = "0.5") + theme(plot.title = element_text(lineheight=.8, face="bold"))
CPI_plot

# Stopa_bez #
pre_IRF = irf(USA_VAR, impulse = "FED_Securities", response = c("Stopa_bez"),n.ahead = 12, cumulative = TRUE,
              seed = 123456)

Stopa_bez_IRF <- data.frame(t = seq(from = 0, to = 12, by = 1),
                      Val_IRF = pre_IRF$irf$FED_Securities,
                      Upp_IRF = pre_IRF$Upper$FED_Securities,
                      Low_IRF = pre_IRF$Lower$FED_Securities)

names(Stopa_bez_IRF) <- c("t", "Val_IRF", "Upp_IRF", "Low_IRF")

Stopa_bez_plot <- ggplot(Stopa_bez_IRF, aes(x = t, y = Val_IRF)) + geom_line(colour = "red", size = 2) + 
  ggtitle("Stopa_bez") + scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + 
  labs(x = 'Okres odpowiedzi na impuls [w miesi¹cach]', y = 'Punkty procentowe') + 
  theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
  theme(axis.title.x = element_text(margin = margin(10,0,0,0))) +
  geom_line(aes(x = t, y = Upp_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = t, y = Low_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = Low_IRF, ymax = Upp_IRF), fill = "lightblue", alpha = "0.5") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

Stopa_bez_plot

# PKB_realne #
pre_IRF = irf(USA_VAR, impulse = "FED_Securities", response = c("PKB_realne"),n.ahead = 12, cumulative = TRUE,
              seed = 123456)

PKB_realne_IRF <- data.frame(t = seq(from = 0, to = 12, by = 1),
                            Val_IRF = pre_IRF$irf$FED_Securities,
                            Upp_IRF = pre_IRF$Upper$FED_Securities,
                            Low_IRF = pre_IRF$Lower$FED_Securities)

names(PKB_realne_IRF) <- c("t", "Val_IRF", "Upp_IRF", "Low_IRF")

PKB_realne_plot <- ggplot(PKB_realne_IRF, aes(x = t, y = Val_IRF)) + geom_line(colour = "red", size = 2) + 
  scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + ggtitle("PKB_realne") +
  labs(x = 'Okres odpowiedzi na impuls [w miesi¹cach]', y = 'Punkty procentowe') + 
  theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
  theme(axis.title.x = element_text(margin = margin(10,0,0,0))) +
  geom_line(aes(x = t, y = Upp_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = t, y = Low_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = Low_IRF, ymax = Upp_IRF), fill = "lightblue", alpha = "0.5")+
  theme(plot.title = element_text(lineheight=.8, face="bold"))
  
PKB_realne_plot

# SP500 #
pre_IRF = irf(USA_VAR, impulse = "FED_Securities", response = c("SP500"),n.ahead = 12, cumulative = TRUE,
              seed = 123456)

SP500_IRF <- data.frame(t = seq(from = 0, to = 12, by = 1),
                             Val_IRF = pre_IRF$irf$FED_Securities,
                             Upp_IRF = pre_IRF$Upper$FED_Securities,
                             Low_IRF = pre_IRF$Lower$FED_Securities)

names(SP500_IRF) <- c("t", "Val_IRF", "Upp_IRF", "Low_IRF")

SP500_plot <- ggplot(SP500_IRF, aes(x = t, y = Val_IRF)) + geom_line(colour = "red", size = 2) + 
  scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + ggtitle("SP500") +
  labs(x = 'Okres odpowiedzi na impuls [w miesi¹cach]', y = 'Punkty indeksowe') + 
  theme(axis.title.y = element_text(margin=margin(0,10,0,0))) +
  theme(axis.title.x = element_text(margin=margin(10,0,0,0))) +
  geom_line(aes(x = t, y = Upp_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = t, y = Low_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = Low_IRF, ymax = Upp_IRF), fill = "lightblue", alpha = "0.5") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))

SP500_plot

# Yield10Gov #
pre_IRF = irf(USA_VAR, impulse = "FED_Securities", response = c("Yield10Gov"),n.ahead = 12, cumulative = TRUE,
              seed = 123456)

Yield10Gov_IRF <- data.frame(t = seq(from = 0, to = 12, by = 1),
                        Val_IRF = pre_IRF$irf$FED_Securities,
                        Upp_IRF = pre_IRF$Upper$FED_Securities,
                        Low_IRF = pre_IRF$Lower$FED_Securities)

names(Yield10Gov_IRF) <- c("t", "Val_IRF", "Upp_IRF", "Low_IRF")

Yield10Gov_plot <- ggplot(Yield10Gov_IRF, aes(x = t, y = Val_IRF)) + geom_line(colour = "red", size = 2) + 
  scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + ggtitle("Yield10Gov") +
  labs(x = 'Okres odpowiedzi na impuls [w miesi¹cach]', y = 'Punkty procentowe') + 
  theme(axis.title.y = element_text(margin=margin(0,10,0,0))) +
  theme(axis.title.x = element_text(margin=margin(10,0,0,0))) +
  geom_line(aes(x = t, y = Upp_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = t, y = Low_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = Low_IRF, ymax = Upp_IRF), fill = "lightblue", alpha = "0.5") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))

Yield10Gov_plot

# Yield1YGov #
pre_IRF = irf(USA_VAR, impulse = "FED_Securities", response = c("Yield1YGov"),n.ahead = 12, cumulative = TRUE,
              seed = 123456)

Yield1YGov_IRF <- data.frame(t = seq(from = 0, to = 12, by = 1),
                             Val_IRF = pre_IRF$irf$FED_Securities,
                             Upp_IRF = pre_IRF$Upper$FED_Securities,
                             Low_IRF = pre_IRF$Lower$FED_Securities)

names(Yield1YGov_IRF) <- c("t", "Val_IRF", "Upp_IRF", "Low_IRF")

Yield1YGov_plot <- ggplot(Yield1YGov_IRF, aes(x = t, y = Val_IRF)) + geom_line(colour = "red", size = 2) + 
  scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + ggtitle("Yield1YGov") +
  labs(x = 'Okres odpowiedzi na impuls [w miesi¹cach]', y = 'Punkty procentowe') + 
  theme(axis.title.y = element_text(margin=margin(0,10,0,0))) +
  theme(axis.title.x = element_text(margin=margin(10,0,0,0))) +
  geom_line(aes(x = t, y = Upp_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = t, y = Low_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = Low_IRF, ymax = Upp_IRF), fill = "lightblue", alpha = "0.5") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))
  
Yield1YGov_plot

# UnempDur #
pre_IRF = irf(USA_VAR, impulse = "FED_Securities", response = c("UnempDur"),n.ahead = 12, cumulative = TRUE,
              seed = 123456)

UnempDur_IRF <- data.frame(t = seq(from = 0, to = 12, by = 1),
                             Val_IRF = pre_IRF$irf$FED_Securities,
                             Upp_IRF = pre_IRF$Upper$FED_Securities,
                             Low_IRF = pre_IRF$Lower$FED_Securities)

names(UnempDur_IRF) <- c("t", "Val_IRF", "Upp_IRF", "Low_IRF")

UnempDur_plot <- ggplot(UnempDur_IRF, aes(x = t, y = Val_IRF)) + geom_line(colour = "red", size = 2) + 
  scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + ggtitle("UnempDur") +
  labs(x = 'Okres odpowiedzi na impuls [w miesi¹cach]', y = 'Tygodnie') + 
  theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
  theme(axis.title.x = element_text(margin = margin(10,0,0,0))) +
  geom_line(aes(x = t, y = Upp_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = t, y = Low_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = Low_IRF, ymax = Upp_IRF), fill = "lightblue", alpha = "0.5") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))

UnempDur_plot

# EURUSD #
pre_IRF = irf(USA_VAR, impulse = "FED_Securities", response = c("EURUSD"),n.ahead = 12, cumulative = TRUE,
              seed = 123456)

EURUSD_IRF <- data.frame(t = seq(from = 0, to = 12, by = 1),
                           Val_IRF = pre_IRF$irf$FED_Securities,
                           Upp_IRF = pre_IRF$Upper$FED_Securities,
                           Low_IRF = pre_IRF$Lower$FED_Securities)

names(EURUSD_IRF) <- c("t", "Val_IRF", "Upp_IRF", "Low_IRF")

EURUSD_plot <- ggplot(EURUSD_IRF, aes(x = t, y = Val_IRF)) + geom_line(colour = "red", size = 2) + 
  scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + ggtitle("EURUSD") +
  labs(x = 'Okres odpowiedzi na impuls [w miesi¹cach]', y = '') + 
  theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
  theme(axis.title.x = element_text(margin = margin(10,0,0,0))) +
  geom_line(aes(x = t, y = Upp_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = t, y = Low_IRF), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = Low_IRF, ymax = Upp_IRF), fill = "lightblue", alpha = "0.5") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))
  
EURUSD_plot

#wykres wszystkich funkcji IRF 
multiplot(CPI_plot, Stopa_bez_plot, PKB_realne_plot, SP500_plot, Yield10Gov_plot, Yield1YGov_plot,
          UnempDur_plot, EURUSD_plot, cols = 2)

################### Odchylenia standardowe i œrednie zmiennych #############

####CPI####
sd(DANE$CPI)
mean(DANE$CPI)

# udzia³ rocznej zmiany w œredniej
-0.4/mean(DANE$CPI) 
# ~ -27 %

#####Stopa_bez######
sd(DANE$Stopa_bez)
mean(DANE$Stopa_bez)

# udzia³ rocznej zmiany w œredniej
-0.15/mean(DANE$Stopa_bez) 
# ~ -2%

#####PKB_realne######
sd(DANE$PKB_realne)
mean(DANE$PKB_realne)

# udzia³ rocznej zmiany w œredniej
0.5/mean(DANE$PKB_realne) 
# ~ 39%

#####SP500######
sd(DANE$SP500)
mean(DANE$SP500)

# udzia³ rocznej zmiany w œredniej
170/mean(DANE$SP500) 
# ~ 11%

######Yield10Gov#####
sd(DANE$Yield10Gov)
mean(DANE$Yield10Gov)

# udzia³ rocznej zmiany w œredniej
0.75/mean(DANE$Yield10Gov)
# ~ 29%

######Yield1YGov######
sd(DANE$Yield1YGov)
mean(DANE$Yield1YGov)

# udzia³ rocznej zmiany w œredniej
-0.06/mean(DANE$Yield1YGov)
# ~ -17%

#####UnempDur######
sd(DANE$UnempDur)
mean(DANE$UnempDur)

# udzia³ rocznej zmiany w œredniej
-0.5/mean(DANE$UnempDur)
# ~ -3%

#####EURUSD#####
sd(DANE$EURUSD)
mean(DANE$EURUSD)

# udzia³ rocznej zmiany w œredniej
  0.025/mean(DANE$EURUSD)
# ~ 2%

######FED_Securities#####
sd(DANE$FED_Securities)
mean(DANE$FED_Securities)

################################ Dekompozycja wariancji b³êdów rocznych prognoz ####################################

# tworzenie zmiennej potrzebnej do dekompozycji wariancji b³êdów rocznych prognoz
FEVD_USA = fevd(USA_VAR, n.ahead = 12)

# Wykresy

# CPI #
FEVD_CPI <- data.frame(FEVD_USA$CPI)

CPI_pre_plots <- data.frame()

CPI_pre_plots <- rbind(data.frame(t = seq(from = 1, to = 12, by = 1), Var = "CPI", Val = FEVD_CPI$CPI),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Stopa_bez", Val = FEVD_CPI$Stopa_bez),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "PKB_realne", Val = FEVD_CPI$PKB_realne),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "FED_Securities", Val = FEVD_CPI$FED_Securities),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield10Gov", Val = FEVD_CPI$Yield10Gov),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield1YGov", Val = FEVD_CPI$Yield1YGov),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "SP500", Val = FEVD_CPI$SP500),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "UnempDur", Val = FEVD_CPI$UnempDur),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "EURUSD", Val = FEVD_CPI$EURUSD))

CPI_FEVD_plot <- ggplot(CPI_pre_plots, aes(x = t, y = Val)) + geom_area(aes(colour = Var, fill = Var),
                 position = 'stack') + ggtitle("CPI")  +
                 scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + 
                 labs(x = 'Okres prognozy [w miesi¹cach]', y = '') + 
                 scale_y_continuous(labels = scales::percent, oob = rescale_none, expand = c(0,0)) +
                 theme(axis.title.x = element_text(margin = margin(10,0,0,0)), legend.title = element_blank(),
                 plot.title = element_text(lineheight = .8, face="bold"), legend.position = "none")
CPI_FEVD_plot

# Stopa_bez #
FEVD_Stopa_bez <- data.frame(FEVD_USA$Stopa_bez)

Stopa_bez_pre_plots <- data.frame()

Stopa_bez_pre_plots <- rbind(data.frame(t = seq(from = 1, to = 12, by = 1), Var = "CPI", Val = FEVD_Stopa_bez$CPI),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Stopa_bez", Val = FEVD_Stopa_bez$Stopa_bez),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "PKB_realne", Val = FEVD_Stopa_bez$PKB_realne),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "FED_Securities", Val = FEVD_Stopa_bez$FED_Securities),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield10Gov", Val = FEVD_Stopa_bez$Yield10Gov),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield1YGov", Val = FEVD_Stopa_bez$Yield1YGov),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "SP500", Val = FEVD_Stopa_bez$SP500),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "UnempDur", Val = FEVD_Stopa_bez$UnempDur),
                       data.frame(t = seq(from = 1, to = 12, by = 1), Var = "EURUSD", Val = FEVD_Stopa_bez$EURUSD))

Stopa_bez_FEVD_plot <- ggplot(Stopa_bez_pre_plots, aes(x = t, y = Val)) + geom_area(aes(colour = Var, fill = Var),
                       position = 'stack') + ggtitle("Stopa_bez") + 
                       scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + 
                       labs(x = 'Okres prognozy [w miesi¹cach]', y = '') + 
                       scale_y_continuous(labels = scales::percent, oob = rescale_none, expand = c(0,0)) +
                       theme(axis.title.x = element_text(margin = margin(10,0,0,0)), legend.title = element_blank(),
                       plot.title = element_text(lineheight = .8, face="bold"), legend.position = "none")

Stopa_bez_FEVD_plot

# PKB_realne #
FEVD_PKB_realne <- data.frame(FEVD_USA$PKB_realne)

PKB_realne_pre_plots <- data.frame()

PKB_realne_pre_plots <- rbind(data.frame(t = seq(from = 1, to = 12, by = 1), Var = "CPI", Val = FEVD_PKB_realne$CPI),
                             data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Stopa_bez", Val = FEVD_PKB_realne$Stopa_bez),
                             data.frame(t = seq(from = 1, to = 12, by = 1), Var = "PKB_realne", Val = FEVD_PKB_realne$PKB_realne),
                             data.frame(t = seq(from = 1, to = 12, by = 1), Var = "FED_Securities", Val = FEVD_PKB_realne$FED_Securities),
                             data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield10Gov", Val = FEVD_PKB_realne$Yield10Gov),
                             data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield1YGov", Val = FEVD_PKB_realne$Yield1YGov),
                             data.frame(t = seq(from = 1, to = 12, by = 1), Var = "SP500", Val = FEVD_PKB_realne$SP500),
                             data.frame(t = seq(from = 1, to = 12, by = 1), Var = "UnempDur", Val = FEVD_PKB_realne$UnempDur),
                             data.frame(t = seq(from = 1, to = 12, by = 1), Var = "EURUSD", Val = FEVD_PKB_realne$EURUSD))

PKB_realne_FEVD_plot <- ggplot(PKB_realne_pre_plots, aes(x = t, y = Val)) + 
                        geom_area(aes(colour = Var, fill = Var), position = 'stack') + ggtitle("PKB_realne") + 
                        scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + 
                        labs(x = 'Okres prognozy [w miesi¹cach]', y = '') + 
                        scale_y_continuous(labels = scales::percent, oob = rescale_none, expand = c(0,0)) +
                        theme(axis.title.x = element_text(margin = margin(10,0,0,0)), legend.title = element_blank(),
                        plot.title = element_text(lineheight = .8, face="bold"), legend.position = "none")

PKB_realne_FEVD_plot

# FED_Securities #
FEVD_FED_Securities <- data.frame(FEVD_USA$FED_Securities)

FED_Securities_pre_plots <- data.frame()

FED_Securities_pre_plots <- rbind(data.frame(t = seq(from = 1, to = 12, by = 1), Var = "CPI", Val = FEVD_FED_Securities$CPI),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Stopa_bez", Val = FEVD_FED_Securities$Stopa_bez),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "PKB_realne", Val = FEVD_FED_Securities$PKB_realne),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "FED_Securities", Val = FEVD_FED_Securities$FED_Securities),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield10Gov", Val = FEVD_FED_Securities$Yield10Gov),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield1YGov", Val = FEVD_FED_Securities$Yield1YGov),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "SP500", Val = FEVD_FED_Securities$SP500),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "UnempDur", Val = FEVD_FED_Securities$UnempDur),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "EURUSD", Val = FEVD_FED_Securities$EURUSD))

FED_Securities_FEVD_plot <- ggplot(FED_Securities_pre_plots, aes(x = t, y = Val)) + 
                            geom_area(aes(colour = Var, fill = Var),                                                                                      position = 'stack') + ggtitle("FED_Securities") + 
                            scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + 
                            labs(x = 'Okres prognozy [w miesi¹cach]', y = '') + 
                            scale_y_continuous(labels = scales::percent, oob = rescale_none, expand = c(0,0)) +
                            theme(axis.title.x = element_text(margin = margin(10,0,0,0)), legend.title = element_blank(),
                            plot.title = element_text(lineheight = .8, face="bold"), legend.position = "none")

FED_Securities_FEVD_plot

# SP500 #
FEVD_SP500 <- data.frame(FEVD_USA$SP500)

SP500_pre_plots <- data.frame()

SP500_pre_plots <- rbind(data.frame(t = seq(from = 1, to = 12, by = 1), Var = "CPI", Val = FEVD_SP500$CPI),
                                  data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Stopa_bez", Val = FEVD_SP500$Stopa_bez),
                                  data.frame(t = seq(from = 1, to = 12, by = 1), Var = "PKB_realne", Val = FEVD_SP500$PKB_realne),
                                  data.frame(t = seq(from = 1, to = 12, by = 1), Var = "FED_Securities", Val = FEVD_SP500$FED_Securities),
                                  data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield10Gov", Val = FEVD_SP500$Yield10Gov),
                                  data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield1YGov", Val = FEVD_SP500$Yield1YGov),
                                  data.frame(t = seq(from = 1, to = 12, by = 1), Var = "SP500", Val = FEVD_SP500$SP500),
                                  data.frame(t = seq(from = 1, to = 12, by = 1), Var = "UnempDur", Val = FEVD_SP500$UnempDur),
                                  data.frame(t = seq(from = 1, to = 12, by = 1), Var = "EURUSD", Val = FEVD_SP500$EURUSD))

SP500_FEVD_plot <- ggplot(SP500_pre_plots, aes(x = t, y = Val)) + 
                   geom_area(aes(colour = Var, fill = Var),                                                                                            position = 'stack') + ggtitle("SP500") + 
                   scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + 
                   labs(x = 'Okres prognozy [w miesi¹cach]', y = '') + 
                   scale_y_continuous(labels = scales::percent, oob = rescale_none, expand = c(0,0)) +
                   theme(axis.title.x = element_text(margin = margin(10,0,0,0)), legend.title = element_blank(),
                   plot.title = element_text(lineheight = .8, face="bold"), legend.position = "none")

SP500_FEVD_plot

# Yield10Gov #
FEVD_Yield10Gov <- data.frame(FEVD_USA$Yield10Gov)

Yield10Gov_pre_plots <- data.frame()

Yield10Gov_pre_plots <- rbind(data.frame(t = seq(from = 1, to = 12, by = 1), Var = "CPI", Val = FEVD_Yield10Gov$CPI),
                         data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Stopa_bez", Val = FEVD_Yield10Gov$Stopa_bez),
                         data.frame(t = seq(from = 1, to = 12, by = 1), Var = "PKB_realne", Val = FEVD_Yield10Gov$PKB_realne),
                         data.frame(t = seq(from = 1, to = 12, by = 1), Var = "FED_Securities", Val = FEVD_Yield10Gov$FED_Securities),
                         data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield10Gov", Val = FEVD_Yield10Gov$Yield10Gov),
                         data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield1YGov", Val = FEVD_Yield10Gov$Yield1YGov),
                         data.frame(t = seq(from = 1, to = 12, by = 1), Var = "SP500", Val = FEVD_Yield10Gov$SP500),
                         data.frame(t = seq(from = 1, to = 12, by = 1), Var = "UnempDur", Val = FEVD_Yield10Gov$UnempDur),
                         data.frame(t = seq(from = 1, to = 12, by = 1), Var = "EURUSD", Val = FEVD_Yield10Gov$EURUSD))

Yield10Gov_FEVD_plot <- ggplot(Yield10Gov_pre_plots, aes(x = t, y = Val)) +
                        geom_area(aes(colour = Var, fill = Var),                                                                            position = 'stack') + ggtitle("Yield10Gov") + 
                        scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + 
                        labs(x = 'Okres prognozy [w miesi¹cach]', y = '') + 
                        scale_y_continuous(labels = scales::percent, oob = rescale_none, expand = c(0,0)) +
                        theme(axis.title.x = element_text(margin = margin(10,0,0,0)), legend.title = element_blank(),
                        plot.title = element_text(lineheight = .8, face="bold"), legend.position = "none")

Yield10Gov_FEVD_plot

# Yield1YGov #
FEVD_Yield1YGov <- data.frame(FEVD_USA$Yield1YGov)

Yield1YGov_pre_plots <- data.frame()

Yield1YGov_pre_plots <- rbind(data.frame(t = seq(from = 1, to = 12, by = 1), Var = "CPI", Val = FEVD_Yield1YGov$CPI),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Stopa_bez", Val = FEVD_Yield1YGov$Stopa_bez),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "PKB_realne", Val = FEVD_Yield1YGov$PKB_realne),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "FED_Securities", Val = FEVD_Yield1YGov$FED_Securities),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield10Gov", Val = FEVD_Yield1YGov$Yield10Gov),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield1YGov", Val = FEVD_Yield1YGov$Yield1YGov),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "SP500", Val = FEVD_Yield1YGov$SP500),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "UnempDur", Val = FEVD_Yield1YGov$UnempDur),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "EURUSD", Val = FEVD_Yield1YGov$EURUSD))

Yield1YGov_FEVD_plot <- ggplot(Yield1YGov_pre_plots, aes(x = t, y = Val)) + 
                        geom_area(aes(colour = Var, fill = Var),                                                                                      position = 'stack') + ggtitle("Yield1YGov") + 
                        scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + 
                        labs(x = 'Okres prognozy [w miesi¹cach]', y = '') + 
                        scale_y_continuous(labels = scales::percent, oob = rescale_none, expand = c(0,0)) +
                        theme(axis.title.x = element_text(margin = margin(10,0,0,0)), legend.title = element_blank(),
                        plot.title = element_text(lineheight = .8, face="bold"), legend.position = "none")

Yield1YGov_FEVD_plot

# UnnempDur #
FEVD_UnempDur <- data.frame(FEVD_USA$UnempDur)

UnempDur_pre_plots <- data.frame()

UnempDur_pre_plots <- rbind(data.frame(t = seq(from = 1, to = 12, by = 1), Var = "CPI", Val = FEVD_UnempDur$CPI),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Stopa_bez", Val = FEVD_UnempDur$Stopa_bez),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "PKB_realne", Val = FEVD_UnempDur$PKB_realne),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "FED_Securities", Val = FEVD_UnempDur$FED_Securities),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield10Gov", Val = FEVD_UnempDur$Yield10Gov),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield1YGov", Val = FEVD_UnempDur$Yield1YGov),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "SP500", Val = FEVD_UnempDur$SP500),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "UnempDur", Val = FEVD_UnempDur$UnempDur),
                              data.frame(t = seq(from = 1, to = 12, by = 1), Var = "EURUSD", Val = FEVD_UnempDur$EURUSD))

UnempDur_FEVD_plot <- ggplot(UnempDur_pre_plots, aes(x = t, y = Val)) +
                      geom_area(aes(colour = Var, fill = Var),                                                                                    position = 'stack') + ggtitle("UnempDur") + 
                      scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + 
                      labs(x = 'Okres prognozy [w miesi¹cach]', y = '') + 
                      scale_y_continuous(labels = scales::percent, oob = rescale_none, expand = c(0,0)) +
                      theme(axis.title.x = element_text(margin = margin(10,0,0,0)), legend.title = element_blank(),
                      plot.title = element_text(lineheight = .8, face="bold"), legend.position = "none")

UnempDur_FEVD_plot

# EURUSD #
FEVD_EURUSD <- data.frame(FEVD_USA$EURUSD)

EURUSD_pre_plots <- data.frame()

EURUSD_pre_plots <- rbind(data.frame(t = seq(from = 1, to = 12, by = 1), Var = "CPI", Val = FEVD_EURUSD$CPI),
                            data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Stopa_bez", Val = FEVD_EURUSD$Stopa_bez),
                            data.frame(t = seq(from = 1, to = 12, by = 1), Var = "PKB_realne", Val = FEVD_EURUSD$PKB_realne),
                            data.frame(t = seq(from = 1, to = 12, by = 1), Var = "FED_Securities", Val = FEVD_EURUSD$FED_Securities),
                            data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield10Gov", Val = FEVD_EURUSD$Yield10Gov),
                            data.frame(t = seq(from = 1, to = 12, by = 1), Var = "Yield1YGov", Val = FEVD_EURUSD$Yield1YGov),
                            data.frame(t = seq(from = 1, to = 12, by = 1), Var = "SP500", Val = FEVD_EURUSD$SP500),
                            data.frame(t = seq(from = 1, to = 12, by = 1), Var = "UnempDur", Val = FEVD_EURUSD$UnempDur),
                            data.frame(t = seq(from = 1, to = 12, by = 1), Var = "EURUSD", Val = FEVD_EURUSD$EURUSD))

EURUSD_FEVD_plot <- ggplot(EURUSD_pre_plots, aes(x = t, y = Val)) +
                    geom_area(aes(colour = Var, fill = Var),                                                                                                    position = 'stack') + ggtitle("EURUSD") + 
                    scale_x_continuous(breaks = 0:12, oob = rescale_none, expand = c(0,0)) + theme_light() + 
                    labs(x = 'Okres prognozy [w miesi¹cach]', y = '') + 
                    scale_y_continuous(labels = scales::percent, oob = rescale_none, expand = c(0,0)) +
                    theme(axis.title.x = element_text(margin = margin(10,0,0,0)), legend.title = element_blank(),
                    plot.title = element_text(lineheight = .8, face="bold"), legend.position = "none")

EURUSD_FEVD_plot

#wykres wszystkich funkcji FEVD 

grid_arrange_shared_legend(CPI_FEVD_plot, Stopa_bez_FEVD_plot, PKB_realne_FEVD_plot, SP500_FEVD_plot, 
                           Yield10Gov_FEVD_plot, Yield1YGov_FEVD_plot, UnempDur_FEVD_plot, EURUSD_FEVD_plot,
                           FED_Securities_FEVD_plot, ncol = 2, nrow = 5)

###############################################################################################################
#######################################   PROGNOZY z MODELU   #################################################
###############################################################################################################

library(lubridate)

DANE_Fin <- read.xlsx("Wsad R.xlsx", sheetIndex = 1) 
#DANE_Fin <- DANE_Fin[DANE_Fin$Data >= as.Date("2007-06-01"),]

DANE_Fin <- DANE_Fin[,-c(8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 21, 22, 25, 26, 27, 28, 29, 30, 31, 32, 33)]


######################################### Backtesty #######################################################
AllDiffs <- data.frame()

Dat1 <- as.Date("2008-06-01")
Dat2 = Dat1
Dat3 = Dat1
year(Dat2) <- year(Dat2) - 8
year(Dat3) <- year(Dat3) + 1
horyzont = 12

for(i in 1:95)
{
  
  if (i > 84){
    
    horyzont = horyzont - 1
    month(Dat3) <- month(Dat3) - 1
  }
  
  DANE_Fin.short <- DANE_Fin[(DANE_Fin$Data > Dat2) & (DANE_Fin$Data <= Dat1),]
  USA_VAR.short <- VAR(DANE_Fin.short[,2:10], p = 3, type = "const")
  USA_VAR.prog <- predict(USA_VAR.short, n.ahead = horyzont, ci = 0.95)
  VAR_forecast <- data.frame(CPI = USA_VAR.prog$fcst$CPI[,c(-2,-3,-4)], 
                             Stopa_bez = USA_VAR.prog$fcst$Stopa_bez[,c(-2,-3,-4)],
                             PKB_realne = USA_VAR.prog$fcst$PKB_realne[,c(-2,-3,-4)],
                             FED_Securities = USA_VAR.prog$fcst$FED_Securities[,c(-2,-3,-4)],
                             Yield10Gov = USA_VAR.prog$fcst$Yield10Gov[,c(-2,-3,-4)],
                             SP500 = USA_VAR.prog$fcst$SP500[,c(-2,-3,-4)],
                             Yield1YGov = USA_VAR.prog$fcst$Yield1YGov[,c(-2,-3,-4)],
                             UnempDur = USA_VAR.prog$fcst$UnempDur[,c(-2,-3,-4)],
                             EURUSD = USA_VAR.prog$fcst$EURUSD[,c(-2,-3,-4)])
  real <- DANE_Fin[(DANE_Fin$Data > Dat1) & (DANE_Fin$Data <= Dat3),-1]
 
  dif = (real - VAR_forecast)^2
  AllDiffs = rbind(AllDiffs,data.frame(dif))
  month(Dat1) <- month(Dat1) + 1
  month(Dat2) <- month(Dat2) + 1
  month(Dat3) <- month(Dat3) + 1
    
}

granice = c(0, 72, 216, 360, 504, 648, 792, 936, 1059, 1074)

NRMSE_ALL <- data.frame()

for (i in 1:9)
{
  NRMSE_ALL = rbind(NRMSE_ALL,data.frame(Year = 2007 + i,
  
  NRMSE_CPI = sqrt(mean(AllDiffs$CPI[(granice[i]+1):granice[i+1]]))/mean(DANE_Fin$CPI),
  
  NRMSE_Stopa_bez = sqrt(mean(AllDiffs$Stopa_bez[(granice[i]+1):granice[i+1]]))/mean(DANE_Fin$Stopa_bez),
  
  NRMSE_PKB_realne = sqrt(mean(AllDiffs$PKB_realne[(granice[i]+1):granice[i+1]]))/mean(DANE_Fin$PKB_realne),
  
  NRMSE_FED_Securities = sqrt(mean(AllDiffs$FED_Securities[(granice[i]+1):granice[i+1]]))/mean(DANE_Fin$FED_Securities),
  
  NRMSE_Yield10Gov = sqrt(mean(AllDiffs$Yield10Gov[(granice[i]+1):granice[i+1]]))/mean(DANE_Fin$Yield10Gov),
  
  NRMSE_SP500 = sqrt(mean(AllDiffs$SP500[(granice[i]+1):granice[i+1]]))/mean(DANE_Fin$SP500),
  
  NRMSE_Yield1YGov = sqrt(mean(AllDiffs$Yield1YGov[(granice[i]+1):granice[i+1]]))/mean(DANE_Fin$Yield1YGov),
  
  NRMSE_UnempDur = sqrt(mean(AllDiffs$UnempDur[(granice[i]+1):granice[i+1]]))/mean(DANE_Fin$UnempDur),
  
  NRMSE_EURUSD = sqrt(mean(AllDiffs$EURUSD[(granice[i]+1):granice[i+1]]))/mean(DANE_Fin$EURUSD)))
}

NRMSE_ALL

#write.csv(NRMSE_ALL, file="NRMSE_ALL.csv", append=TRUE, sep=",")

rowSums(NRMSE_ALL[,2:10])

rowSums(NRMSE_ALL[,2:10])/9  #widaæ spadek b³êdów dla kolejnych lat

' jak widaæ model daje najlepsze prognozy dla dalszych lat, ostatecznie w 2016 roku osi¹gajac œrednie odchylenie
od œrednich danych zmiennych równe 20%, co pozwala nam na skorzystanie z niego w celu prognozowania rozwoju zmiennych
w rocznaj perspektywie t.j. dla 3Q2016-2Q2017'

################################################################################################################
######################## Roczne prognozy dla wszystkich zmiennych na 3Q2016-2Q2017 #############################
################################################################################################################

# odrzucamy ostatnie 12 obserwacji
DANE_Fin.short <- DANE_Fin[DANE_Fin$Data >= as.Date("2008-06-15"),]

USA_VAR.short <- VAR(DANE_Fin.short[,2:10], p = 3, type = "const")

# tworzymy roczna prognozy
USA_VAR.prog <- predict(USA_VAR.short, n.ahead = 12, ci = 0.95)

####### Prognoza dla CPI
CPI_forecast <- data.frame(fcst = tail(DANE_Fin$CPI,1), lower = NA, upper = NA)
CPI_forecast <- rbind(CPI_forecast,data.frame(USA_VAR.prog$fcst$CPI[,-4])) # wyrzucamy ostatni¹ kolumnê z CI
names(CPI_forecast) <- c("CPI_fore", "CPI_lower", "CPI_upper")
DANE_Fin.short2 <- DANE_Fin[DANE_Fin$Data >= as.Date("2008-06-01"),]
Fdate <- DANE_Fin.short2$Data
Real_Data <- DANE_Fin.short2$CPI
last <- tail(Fdate,1)
month(last) <- month(last) + 1

CPI_ALL <- data.frame(Fdate = Fdate, Real = Real_Data, Upper = NA, Forc = NA, Lower = NA)
CPI_ALL$Upper[length(CPI_ALL$Upper)] = CPI_ALL$Real[length(CPI_ALL$Real)] 
CPI_ALL$Forc[length(CPI_ALL$Forc)] = CPI_ALL$Real[length(CPI_ALL$Real)]
CPI_ALL$Lower[length(CPI_ALL$Lower)] = CPI_ALL$Real[length(CPI_ALL$Real)] 

for( i in 1:12){
  CPI_ALL <- rbind(CPI_ALL, data.frame(Fdate = last, Real = NA, Upper = CPI_forecast$CPI_upper[i+1],
                   Forc = CPI_forecast$CPI_fore[i+1], Lower = CPI_forecast$CPI_lower[i+1]))

  month(last) <- month(last) + 1
}

# CPI wykres prognozy
CPI_prog <- ggplot(CPI_ALL, aes(x = Fdate, y = Real)) + geom_line(colour = "darkblue", size = 1) +
  ggtitle("CPI") + theme_light() + 
  scale_x_date(breaks = date_breaks("year"), labels = date_format("%Y"), expand=c(0,0)) + 
  labs(y = 'Punkty procentowe', x='') + 
  theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
  geom_line(aes(x = CPI_ALL$Fdate, y = CPI_ALL$Upper), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = CPI_ALL$Fdate, y = CPI_ALL$Forc), colour = "red", linetype = 1, size = 1) +
  geom_line(aes(x = CPI_ALL$Fdate, y = CPI_ALL$Lower), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = CPI_ALL$Lower, ymax = CPI_ALL$Upper), fill = "lightblue", alpha = "0.5") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

CPI_prog

######### Prognoza dla Stopa_bez
Stopa_bez_forecast <- data.frame(fcst = tail(DANE_Fin$Stopa_bez,1), lower = NA, upper = NA)
Stopa_bez_forecast <- rbind(Stopa_bez_forecast,data.frame(USA_VAR.prog$fcst$Stopa_bez[,-4])) # wyrzucamy ostatni¹ kolumnê z CI
names(Stopa_bez_forecast) <- c("Stopa_bez_fore", "Stopa_bez_lower", "Stopa_bez_upper")
DANE_Fin.short2 <- DANE_Fin[DANE_Fin$Data >= as.Date("2008-06-01"),]
Fdate <- DANE_Fin.short2$Data
Real_Data <- DANE_Fin.short2$Stopa_bez
last <- tail(Fdate,1)
month(last) <- month(last) + 1

Stopa_bez_ALL <- data.frame(Fdate = Fdate, Real = Real_Data, Upper = NA, Forc = NA, Lower = NA)
Stopa_bez_ALL$Upper[length(Stopa_bez_ALL$Upper)] = Stopa_bez_ALL$Real[length(Stopa_bez_ALL$Real)] 
Stopa_bez_ALL$Forc[length(Stopa_bez_ALL$Forc)] = Stopa_bez_ALL$Real[length(Stopa_bez_ALL$Real)]
Stopa_bez_ALL$Lower[length(Stopa_bez_ALL$Lower)] = Stopa_bez_ALL$Real[length(Stopa_bez_ALL$Real)] 

for( i in 1:12){
  Stopa_bez_ALL <- rbind(Stopa_bez_ALL, data.frame(Fdate = last, Real = NA, Upper = Stopa_bez_forecast$Stopa_bez_upper[i+1],
                                       Forc = Stopa_bez_forecast$Stopa_bez_fore[i+1], Lower = Stopa_bez_forecast$Stopa_bez_lower[i+1]))
  
  month(last) <- month(last) + 1
}

# Stopa_bez wykres prognozy
Stopa_bez_prog <- ggplot(Stopa_bez_ALL, aes(x = Fdate, y = Real)) + geom_line(colour = "darkblue", size = 1) +
  ggtitle("Stopa_bez") + theme_light() + 
  scale_x_date(breaks = date_breaks("year"), labels = date_format("%Y"), expand=c(0,0)) + 
  labs(y = 'Punkty procentowe', x='') + 
  theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
  geom_line(aes(x = Stopa_bez_ALL$Fdate, y = Stopa_bez_ALL$Upper), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = Stopa_bez_ALL$Fdate, y = Stopa_bez_ALL$Forc), colour = "red", linetype = 1, size = 1) +
  geom_line(aes(x = Stopa_bez_ALL$Fdate, y = Stopa_bez_ALL$Lower), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = Stopa_bez_ALL$Lower, ymax = Stopa_bez_ALL$Upper), fill = "lightblue", alpha = "0.5") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

Stopa_bez_prog

########## Prognoza dla PKB_realne
PKB_realne_forecast <- data.frame(fcst = tail(DANE_Fin$PKB_realne,1), lower = NA, upper = NA)
PKB_realne_forecast <- rbind(PKB_realne_forecast,data.frame(USA_VAR.prog$fcst$PKB_realne[,-4])) # wyrzucamy ostatni¹ kolumnê z CI
names(PKB_realne_forecast) <- c("PKB_realne_fore", "PKB_realne_lower", "PKB_realne_upper")
DANE_Fin.short2 <- DANE_Fin[DANE_Fin$Data >= as.Date("2008-06-01"),]
Fdate <- DANE_Fin.short2$Data
Real_Data <- DANE_Fin.short2$PKB_realne
last <- tail(Fdate,1)
month(last) <- month(last) + 1

PKB_realne_ALL <- data.frame(Fdate = Fdate, Real = Real_Data, Upper = NA, Forc = NA, Lower = NA)
PKB_realne_ALL$Upper[length(PKB_realne_ALL$Upper)] = PKB_realne_ALL$Real[length(PKB_realne_ALL$Real)] 
PKB_realne_ALL$Forc[length(PKB_realne_ALL$Forc)] = PKB_realne_ALL$Real[length(PKB_realne_ALL$Real)]
PKB_realne_ALL$Lower[length(PKB_realne_ALL$Lower)] = PKB_realne_ALL$Real[length(PKB_realne_ALL$Real)] 

for( i in 1:12){
  PKB_realne_ALL <- rbind(PKB_realne_ALL, data.frame(Fdate = last, Real = NA, Upper = PKB_realne_forecast$PKB_realne_upper[i+1],
                                                   Forc = PKB_realne_forecast$PKB_realne_fore[i+1], Lower = PKB_realne_forecast$PKB_realne_lower[i+1]))
  
  month(last) <- month(last) + 1
}

# PKB_realne wykres prognozy
PKB_realne_prog <- ggplot(PKB_realne_ALL, aes(x = Fdate, y = Real)) + geom_line(colour = "darkblue", size = 1) +
  ggtitle("PKB_realne") + theme_light() + 
  scale_x_date(breaks = date_breaks("year"), labels = date_format("%Y"), expand=c(0,0)) + 
  labs(y = 'Punkty procentowe', x='') + 
  theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
  geom_line(aes(x = PKB_realne_ALL$Fdate, y = PKB_realne_ALL$Upper), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = PKB_realne_ALL$Fdate, y = PKB_realne_ALL$Forc), colour = "red", linetype = 1, size = 1) +
  geom_line(aes(x = PKB_realne_ALL$Fdate, y = PKB_realne_ALL$Lower), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = PKB_realne_ALL$Lower, ymax = PKB_realne_ALL$Upper), fill = "lightblue", alpha = "0.5") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

PKB_realne_prog

######## Prognoza dla FED_Securities
FED_Securities_forecast <- data.frame(fcst = tail(DANE_Fin$FED_Securities,1), lower = NA, upper = NA)
FED_Securities_forecast <- rbind(FED_Securities_forecast,data.frame(USA_VAR.prog$fcst$FED_Securities[,-4])) # wyrzucamy ostatni¹ kolumnê z CI
names(FED_Securities_forecast) <- c("FED_Securities_fore", "FED_Securities_lower", "FED_Securities_upper")
DANE_Fin.short2 <- DANE_Fin[DANE_Fin$Data >= as.Date("2008-06-01"),]
Fdate <- DANE_Fin.short2$Data
Real_Data <- DANE_Fin.short2$FED_Securities
last <- tail(Fdate,1)
month(last) <- month(last) + 1

FED_Securities_ALL <- data.frame(Fdate = Fdate, Real = Real_Data, Upper = NA, Forc = NA, Lower = NA)
FED_Securities_ALL$Upper[length(FED_Securities_ALL$Upper)] = FED_Securities_ALL$Real[length(FED_Securities_ALL$Real)] 
FED_Securities_ALL$Forc[length(FED_Securities_ALL$Forc)] = FED_Securities_ALL$Real[length(FED_Securities_ALL$Real)]
FED_Securities_ALL$Lower[length(FED_Securities_ALL$Lower)] = FED_Securities_ALL$Real[length(FED_Securities_ALL$Real)] 

for( i in 1:12){
  FED_Securities_ALL <- rbind(FED_Securities_ALL, data.frame(Fdate = last, Real = NA, Upper = FED_Securities_forecast$FED_Securities_upper[i+1],
                                                     Forc = FED_Securities_forecast$FED_Securities_fore[i+1], Lower = FED_Securities_forecast$FED_Securities_lower[i+1]))
  
  month(last) <- month(last) + 1
}

# FED_Securities wykres prognozy
FED_Securities_prog <- ggplot(FED_Securities_ALL, aes(x = Fdate, y = Real)) + geom_line(colour = "darkblue", size = 1) +
  ggtitle("FED_Securities") + theme_light() + 
  scale_x_date(breaks = date_breaks("year"), labels = date_format("%Y"), expand=c(0,0)) + 
  labs(y = 'Miliony dolarów', x='') + theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
  geom_line(aes(x = FED_Securities_ALL$Fdate, y = FED_Securities_ALL$Upper), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = FED_Securities_ALL$Fdate, y = FED_Securities_ALL$Forc), colour = "red", linetype = 1, size = 1) +
  geom_line(aes(x = FED_Securities_ALL$Fdate, y = FED_Securities_ALL$Lower), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = FED_Securities_ALL$Lower, ymax = FED_Securities_ALL$Upper), fill = "lightblue", alpha = "0.5") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

FED_Securities_prog

######### Prognoza dla Yield10Gov
Yield10Gov_forecast <- data.frame(fcst = tail(DANE_Fin$Yield10Gov,1), lower = NA, upper = NA)
Yield10Gov_forecast <- rbind(Yield10Gov_forecast,data.frame(USA_VAR.prog$fcst$Yield10Gov[,-4])) # wyrzucamy ostatni¹ kolumnê z CI
names(Yield10Gov_forecast) <- c("Yield10Gov_fore", "Yield10Gov_lower", "Yield10Gov_upper")
DANE_Fin.short2 <- DANE_Fin[DANE_Fin$Data >= as.Date("2008-06-01"),]
Fdate <- DANE_Fin.short2$Data
Real_Data <- DANE_Fin.short2$Yield10Gov
last <- tail(Fdate,1)
month(last) <- month(last) + 1

Yield10Gov_ALL <- data.frame(Fdate = Fdate, Real = Real_Data, Upper = NA, Forc = NA, Lower = NA)
Yield10Gov_ALL$Upper[length(Yield10Gov_ALL$Upper)] = Yield10Gov_ALL$Real[length(Yield10Gov_ALL$Real)] 
Yield10Gov_ALL$Forc[length(Yield10Gov_ALL$Forc)] = Yield10Gov_ALL$Real[length(Yield10Gov_ALL$Real)]
Yield10Gov_ALL$Lower[length(Yield10Gov_ALL$Lower)] = Yield10Gov_ALL$Real[length(Yield10Gov_ALL$Real)] 

for( i in 1:12){
  Yield10Gov_ALL <- rbind(Yield10Gov_ALL, data.frame(Fdate = last, Real = NA, Upper = Yield10Gov_forecast$Yield10Gov_upper[i+1],
                                                             Forc = Yield10Gov_forecast$Yield10Gov_fore[i+1], Lower = Yield10Gov_forecast$Yield10Gov_lower[i+1]))
  
  month(last) <- month(last) + 1
}

# Yield10Gov wykres prognozy
Yield10Gov_prog <- ggplot(Yield10Gov_ALL, aes(x = Fdate, y = Real)) + geom_line(colour = "darkblue", size = 1) +
  ggtitle("Yield10Gov") + theme_light() + 
  scale_x_date(breaks = date_breaks("year"), labels = date_format("%Y"), expand=c(0,0)) + 
  labs(y = 'Punkty procentowe', x='') + 
  theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
  geom_line(aes(x = Yield10Gov_ALL$Fdate, y = Yield10Gov_ALL$Upper), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = Yield10Gov_ALL$Fdate, y = Yield10Gov_ALL$Forc), colour = "red", linetype = 1, size = 1) +
  geom_line(aes(x = Yield10Gov_ALL$Fdate, y = Yield10Gov_ALL$Lower), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = Yield10Gov_ALL$Lower, ymax = Yield10Gov_ALL$Upper), fill = "lightblue", alpha = "0.5") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

Yield10Gov_prog

######### Prognoza dla Yield1YGov
Yield1YGov_forecast <- data.frame(fcst = tail(DANE_Fin$Yield1YGov,1), lower = NA, upper = NA)
Yield1YGov_forecast <- rbind(Yield1YGov_forecast,data.frame(USA_VAR.prog$fcst$Yield1YGov[,-4])) # wyrzucamy ostatni¹ kolumnê z CI
names(Yield1YGov_forecast) <- c("Yield1YGov_fore", "Yield1YGov_lower", "Yield1YGov_upper")
DANE_Fin.short2 <- DANE_Fin[DANE_Fin$Data >= as.Date("2008-06-01"),]
Fdate <- DANE_Fin.short2$Data
Real_Data <- DANE_Fin.short2$Yield1YGov
last <- tail(Fdate,1)
month(last) <- month(last) + 1

Yield1YGov_ALL <- data.frame(Fdate = Fdate, Real = Real_Data, Upper = NA, Forc = NA, Lower = NA)
Yield1YGov_ALL$Upper[length(Yield1YGov_ALL$Upper)] = Yield1YGov_ALL$Real[length(Yield1YGov_ALL$Real)] 
Yield1YGov_ALL$Forc[length(Yield1YGov_ALL$Forc)] = Yield1YGov_ALL$Real[length(Yield1YGov_ALL$Real)]
Yield1YGov_ALL$Lower[length(Yield1YGov_ALL$Lower)] = Yield1YGov_ALL$Real[length(Yield1YGov_ALL$Real)] 

for( i in 1:12){
  Yield1YGov_ALL <- rbind(Yield1YGov_ALL, data.frame(Fdate = last, Real = NA, Upper = Yield1YGov_forecast$Yield1YGov_upper[i+1],
                                                     Forc = Yield1YGov_forecast$Yield1YGov_fore[i+1], Lower = Yield1YGov_forecast$Yield1YGov_lower[i+1]))
  
  month(last) <- month(last) + 1
}

# Yield1YGov wykres prognozy
Yield1YGov_prog <- ggplot(Yield1YGov_ALL, aes(x = Fdate, y = Real)) + geom_line(colour = "darkblue", size = 1) +
  ggtitle("Yield1YGov") + theme_light() + 
  scale_x_date(breaks = date_breaks("year"), labels = date_format("%Y"), expand=c(0,0)) + 
  labs(y = 'Punkty procentowe', x='') + 
  theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
  geom_line(aes(x = Yield1YGov_ALL$Fdate, y = Yield1YGov_ALL$Upper), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = Yield1YGov_ALL$Fdate, y = Yield1YGov_ALL$Forc), colour = "red", linetype = 1, size = 1) +
  geom_line(aes(x = Yield1YGov_ALL$Fdate, y = Yield1YGov_ALL$Lower), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = Yield1YGov_ALL$Lower, ymax = Yield1YGov_ALL$Upper), fill = "lightblue", alpha = "0.5") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

Yield1YGov_prog

######## Prognoza dla SP500
SP500_forecast <- data.frame(fcst = tail(DANE_Fin$SP500,1), lower = NA, upper = NA)
SP500_forecast <- rbind(SP500_forecast,data.frame(USA_VAR.prog$fcst$SP500[,-4])) # wyrzucamy ostatni¹ kolumnê z CI
names(SP500_forecast) <- c("SP500_fore", "SP500_lower", "SP500_upper")
DANE_Fin.short2 <- DANE_Fin[DANE_Fin$Data >= as.Date("2008-06-01"),]
Fdate <- DANE_Fin.short2$Data
Real_Data <- DANE_Fin.short2$SP500
last <- tail(Fdate,1)
month(last) <- month(last) + 1

SP500_ALL <- data.frame(Fdate = Fdate, Real = Real_Data, Upper = NA, Forc = NA, Lower = NA)
SP500_ALL$Upper[length(SP500_ALL$Upper)] = SP500_ALL$Real[length(SP500_ALL$Real)] 
SP500_ALL$Forc[length(SP500_ALL$Forc)] = SP500_ALL$Real[length(SP500_ALL$Real)]
SP500_ALL$Lower[length(SP500_ALL$Lower)] = SP500_ALL$Real[length(SP500_ALL$Real)] 

for( i in 1:12){
  SP500_ALL <- rbind(SP500_ALL, data.frame(Fdate = last, Real = NA, Upper = SP500_forecast$SP500_upper[i+1],
                                                     Forc = SP500_forecast$SP500_fore[i+1], Lower = SP500_forecast$SP500_lower[i+1]))
  
  month(last) <- month(last) + 1
}

# SP500 wykres prognozy
SP500_prog <- ggplot(SP500_ALL, aes(x = Fdate, y = Real)) + geom_line(colour = "darkblue", size = 1) +
  ggtitle("SP500") + theme_light() + 
  scale_x_date(breaks = date_breaks("year"), labels = date_format("%Y"), expand=c(0,0)) + 
  labs(y = 'Punkty indeksowe', x='') +
  theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
  geom_line(aes(x = SP500_ALL$Fdate, y = SP500_ALL$Upper), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = SP500_ALL$Fdate, y = SP500_ALL$Forc), colour = "red", linetype = 1, size = 1) +
  geom_line(aes(x = SP500_ALL$Fdate, y = SP500_ALL$Lower), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = SP500_ALL$Lower, ymax = SP500_ALL$Upper), fill = "lightblue", alpha = "0.5") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

SP500_prog

######## Prognoza dla UnempDur
UnempDur_forecast <- data.frame(fcst = tail(DANE_Fin$UnempDur,1), lower = NA, upper = NA)
UnempDur_forecast <- rbind(UnempDur_forecast,data.frame(USA_VAR.prog$fcst$UnempDur[,-4])) # wyrzucamy ostatni¹ kolumnê z CI
names(UnempDur_forecast) <- c("UnempDur_fore", "UnempDur_lower", "UnempDur_upper")
DANE_Fin.short2 <- DANE_Fin[DANE_Fin$Data >= as.Date("2008-06-01"),]
Fdate <- DANE_Fin.short2$Data
Real_Data <- DANE_Fin.short2$UnempDur
last <- tail(Fdate,1)
month(last) <- month(last) + 1

UnempDur_ALL <- data.frame(Fdate = Fdate, Real = Real_Data, Upper = NA, Forc = NA, Lower = NA)
UnempDur_ALL$Upper[length(UnempDur_ALL$Upper)] = UnempDur_ALL$Real[length(UnempDur_ALL$Real)] 
UnempDur_ALL$Forc[length(UnempDur_ALL$Forc)] = UnempDur_ALL$Real[length(UnempDur_ALL$Real)]
UnempDur_ALL$Lower[length(UnempDur_ALL$Lower)] = UnempDur_ALL$Real[length(UnempDur_ALL$Real)] 

for( i in 1:12){
  UnempDur_ALL <- rbind(UnempDur_ALL, data.frame(Fdate = last, Real = NA, Upper = UnempDur_forecast$UnempDur_upper[i+1],
                                           Forc = UnempDur_forecast$UnempDur_fore[i+1], Lower = UnempDur_forecast$UnempDur_lower[i+1]))
  
  month(last) <- month(last) + 1
}

# UnempDur wykres prognozy
UnempDur_prog <- ggplot(UnempDur_ALL, aes(x = Fdate, y = Real)) + geom_line(colour = "darkblue", size = 1) +
  ggtitle("UnempDur") + theme_light() + 
  scale_x_date(breaks = date_breaks("year"), labels = date_format("%Y"), expand=c(0,0)) + 
  labs(y = 'Tygodnie', x='') + theme(axis.title.y = element_text(margin = margin(0,10,0,0))) +
  geom_line(aes(x = UnempDur_ALL$Fdate, y = UnempDur_ALL$Upper), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = UnempDur_ALL$Fdate, y = UnempDur_ALL$Forc), colour = "red", linetype = 1, size = 1) +
  geom_line(aes(x = UnempDur_ALL$Fdate, y = UnempDur_ALL$Lower), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = UnempDur_ALL$Lower, ymax = UnempDur_ALL$Upper), fill = "lightblue", alpha = "0.5") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

UnempDur_prog

########### Prognoza dla EURUSD
EURUSD_forecast <- data.frame(fcst = tail(DANE_Fin$EURUSD,1), lower = NA, upper = NA)
EURUSD_forecast <- rbind(EURUSD_forecast,data.frame(USA_VAR.prog$fcst$EURUSD[,-4])) # wyrzucamy ostatni¹ kolumnê z CI
names(EURUSD_forecast) <- c("EURUSD_fore", "EURUSD_lower", "EURUSD_upper")
DANE_Fin.short2 <- DANE_Fin[DANE_Fin$Data >= as.Date("2008-06-01"),]
Fdate <- DANE_Fin.short2$Data
Real_Data <- DANE_Fin.short2$EURUSD
last <- tail(Fdate,1)
month(last) <- month(last) + 1

EURUSD_ALL <- data.frame(Fdate = Fdate, Real = Real_Data, Upper = NA, Forc = NA, Lower = NA)
EURUSD_ALL$Upper[length(EURUSD_ALL$Upper)] = EURUSD_ALL$Real[length(EURUSD_ALL$Real)] 
EURUSD_ALL$Forc[length(EURUSD_ALL$Forc)] = EURUSD_ALL$Real[length(EURUSD_ALL$Real)]
EURUSD_ALL$Lower[length(EURUSD_ALL$Lower)] = EURUSD_ALL$Real[length(EURUSD_ALL$Real)] 

for( i in 1:12){
  EURUSD_ALL <- rbind(EURUSD_ALL, data.frame(Fdate = last, Real = NA, Upper = EURUSD_forecast$EURUSD_upper[i+1],
                                                 Forc = EURUSD_forecast$EURUSD_fore[i+1], Lower = EURUSD_forecast$EURUSD_lower[i+1]))
  
  month(last) <- month(last) + 1
}

# EURUSD wykres prognozy
EURUSD_prog <- ggplot(EURUSD_ALL, aes(x = Fdate, y = Real)) + geom_line(colour = "darkblue", size = 1) +
  ggtitle("EURUSD") + theme_light() + 
  scale_x_date(breaks = date_breaks("year"), labels = date_format("%Y"), expand=c(0,0)) + labs(y = '', x='') + 
  geom_line(aes(x = EURUSD_ALL$Fdate, y = EURUSD_ALL$Upper), colour = "darkblue", linetype = 2, size = 1) +
  geom_line(aes(x = EURUSD_ALL$Fdate, y = EURUSD_ALL$Forc), colour = "red", linetype = 1, size = 1) +
  geom_line(aes(x = EURUSD_ALL$Fdate, y = EURUSD_ALL$Lower), colour = "darkblue", linetype = 2, size = 1) +
  geom_ribbon(aes(ymin = EURUSD_ALL$Lower, ymax = EURUSD_ALL$Upper), fill = "lightblue", alpha = "0.5") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"), axis.title.y = element_text(margin = margin(0,10,0,0)))

EURUSD_prog

#wykres wszystkich prognoz
multiplot(CPI_prog, Stopa_bez_prog, PKB_realne_prog, SP500_prog, Yield10Gov_prog, Yield1YGov_prog,
          UnempDur_prog, EURUSD_prog, FED_Securities_prog, cols = 2)

####################################################################################################################