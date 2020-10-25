

################### 2. Wo sind die Daten gespeichert?########################################
##Import raw data as a dataframe (table) named df
### setwd("~/_Lehren/R_lehren")

## zweite Möglichkeit
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(this.dir)


################### 3. Tab 



################### 4. Was ist ein Objekt und seine Eigenschaft?
x <- 1
x1 <- "1"
y <- 0:100

#### Einfache Mathe ####

x*5
test <- y*5

###################  5. Was ist eine Funktion?

### Mittelwert
mean(y)
### Standardabweichung
sd(y)
### Summe
sum(y)
### Beispiel
LSD.test()

###################  6. Was ist ein Packet?
## Beispiel-Packet agricolae
library(agricolae)


###################  7. Die Daten einlesen/importieren (read.table(); read.csv() etc.)

## reading the data
df <- read.table("Daten_Radieschen_2004.csv",sep=";",dec=".",h=T)
klima <- read.csv("Klima.csv",sep=";",dec=".",h=T)


################### 8. Die Struktur der Daten prüfen (int, num, chr, Factor, Date.) und ändern
## Namen der Spalte
str(df)
## The first rows of the data
head(df)
## example: as.Date, the part "%Y-%m-%d" is adapted by the form of the data
df$Datum <- as.Date(df$Datum, "%Y-%m-%d")
df$Variante <- as.factor(paste("W", df$Variante, sep=""))

klima$Date <- as.Date(klima$Date, "%d.%m.%Y")

################### 9. Was ist ein "data frame"
################### 10. Datenbearbeitung in R
# Neue Spalte

# Rechnen
df$TGGesamt <- df$TGBlatt + df$TGKnolle
DOT <- as.Date("2004-11-04")
df$DAT <- df$Datum-DOT

## Berechung der Thermalzeit und Temperatursumme

### 1. Lineares Modell mit Basistemperatur
## Basistemperatur
Tbase <- 3.5
## Berechnung der Thermalzeit °Cd
klima$Thermalzeit1 <- klima$Temperature-Tbase
## Berechnung der Temperatursumme
klima$TS1 <- cumsum(klima$Thermalzeit1)


###################  11. Wie mache ich eine Abbildung in R?
## Klimadaten plotten
plot(klima$Temperature~klima$Date)
plot(Temperature~Date, data=klima)
plot(Temperature~Date, data=klima, ylim=c(0,30))
plot(Temperature~Date, data=klima, ylim=c(0,30), type="b")
plot(Temperature~Date, data=klima, ylim=c(0,30), type="b")
### Graphische Parameter in R
# https://www.statmethods.net/advgraphs/parameters.html
plot(Temperature~Date, data=klima, ylim=c(0,30), type="b", pch=6)
plot(Temperature~Date, data=klima, ylim=c(0,35), type="b", pch=6, col="blue")
### Linie
abline(Tbase, 0, col="red")

min(klima$Temperature)
max(klima$Temperature)

### 2. Knickpunkt-Modell für die Berechung der Thermalzeit und Temperatursumme
Tbase <- 3.5
Topt <- 21.6
Tmax <- 32.1

klima$Thermalzeit2 <- ifelse(klima$Temperature<Topt, ## if
                             Topt*(klima$Temperature-Tbase)/(Topt-Tbase),            ## yes
                             Topt*(Tmax-klima$Temperature)/(Tmax-Topt)              ## no
                             )
klima$TS2 <- cumsum(klima$Thermalzeit2)

plot(Thermalzeit1~Date, data=klima, ylim=c(0,35), type="b", pch=6)
points(Thermalzeit2~Date, data=klima, col="blue")
lines(Thermalzeit2~Date, data=klima, col="blue")

plot(Thermalzeit2~Thermalzeit1, data=klima)
## 1 to 1 line
abline(0,1)


### 3. Arrhenius-type response curves (Ansprechkurve) für die Berechung der Thermalzeit und Temperatursumme
## Parameter for effective Temperature sum (Parent et al., 2012, Cauliflower)
deltaH=63600
A=937088859.183916
alpha = 3.5
R = 8.314
K = 273
T0=297.4

## Die Kurve checken
x <- seq(0, 40, by=0.1)
y <-A*(x+K)*exp(-deltaH/(R*(x+K)))/(1+exp(-deltaH/(R*(x+K)))^(alpha*(1-(x+K)/T0)))
plot(y~x, col="red")

klima$Thermalzeit3 <- A*(klima$Temperature+K)*exp(-deltaH/(R*(klima$Temperature+K)))/(1+exp(-deltaH/(R*(klima$Temperature+K)))^(alpha*(1-(klima$Temperature+K)/T0)))
klima$TS3 <- cumsum(klima$Thermalzeit3)
## Checken
plot(klima$Thermalzeit3~klima$Thermalzeit1)

#Vorsicht mit Überschreiben!
###################Hilfe!! Hilfe!!

# Googeln
# ? in Console
## ?? in Console

###################################################################################
################################### Zweite Sitzung#################################
###################################################################################
##
df[1, 1]
## subset W100
dfW100 <- subset(df,Variante=="W100")
## Subset W75 und W50
dfW75 <- subset(df,Variante=="W75")
dfW50 <- subset(df,Variante=="W50")

## Daten anschauen
plot(TGGesamt~DAT, col=Variante, data=df, xlim=c(0,40), ylim=c(0,3))

## Überschrift
## Beschriftung
## Export-Abbildung
## lineare Regression
fit <- lm(TGGesamt~DAT, data=df)

## summary: lineare Wachstumsrate
summary(fit)
## abline für die Abbildung
abline(fit)

## Konfidenzinterval

## Frage: Wie kann ich die Wachstumsrate zwischen Behandlungen vergleichen??

##########################################################################################
### Zweite Möglichkeit, Wachstumsrate zu berechnen:

## Schritt 1: Mittelwert berechnen
## apply a function(x) to variables v1 and v2, for each combinations in column c1, c2 and c3 seperately
## x <- aggregate(df[,c("v1", "v2")], by=df[,c("c1", "c2", "c3")], function(x){mean(na.omit(x))})
Data <- aggregate(df[,4:6], by=df[,c("Datum","Variante", "DAT")], function(x){mean(na.omit(x))})
## Wachstumsrate berechnen:
## Für W100:
DataW100<- subset(Data, Variante=="W100")

library(zoo)
DataW100$dDate <- rollapply(DataW100$Datum,  width = 2, FUN = diff.Date, fill=NA, align='right')
## difference in whole dry mass
DataW100$dGesamt <- rollapply(DataW100$TGGesamt,  width = 2, FUN = diff, fill=NA, align='right')
DataW100$Wachstumsrate <- DataW100$dGesamt/DataW100$dDate

##########################################################################################
### Partitionierung
DataW100$dBlatt <- rollapply(DataW100$TGBlatt,  width = 2, FUN = diff, fill=NA, align='right')
DataW100$Par_Blatt <- DataW100$dBlatt/DataW100$dGesamt

## Frage: Wie kann ich die Partitionierung zwischen Behandlungen vergleichen??

DataW50<- subset(Data, Variante=="W50")
DataW50$dDate <- rollapply(DataW50$Datum,  width = 2, FUN = diff.Date, fill=NA, align='right')
## difference in whole dry mass
DataW50$dGesamt <- rollapply(DataW50$TGGesamt,  width = 2, FUN = diff, fill=NA, align='right')
DataW50$Wachstumsrate <- DataW50$dGesamt/DataW50$dDate

##########################################################################################
### Partitionierung
DataW50$dBlatt <- rollapply(DataW50$TGBlatt,  width = 2, FUN = diff, fill=NA, align='right')
DataW50$Par_Blatt <- DataW50$dBlatt/DataW50$dGesamt

##Rbind
Data_all <- rbind(DataW100, DataW50)


### Vergleich Wachstumsrate 
### Beispiel mit do.call
XXXXX = do.call('rbind',lapply(split(df, df$Variante, drop=TRUE), function(x) {
    ## The structure of the data
    ## using "split" to structure the data in small sets
    #x <-split(df, df$Datum, drop=TRUE)[[1]]
    #x <-split(df, list(df$Variante, df$Wiederholung), drop=TRUE)[[1]]
    # x <- split(df, df$Variante, drop=TRUE)[[1]]    ### Testing if thins works

  x
}))


Data_all = do.call('rbind',lapply(split(df, list(df$Variante, df$Wiederholung), drop=TRUE), function(x) {
  # x <- split(df, list(df$Variante, df$Wiederholung))[[1]]    ### Testing if thins works
  x$dDate <- rollapply(x$Datum,  width = 2, FUN = diff.Date, fill=NA, align='right')
  ## difference in whole dry mass
  x$dGesamt <- rollapply(x$TGGesamt,  width = 2, FUN = diff, fill=NA, align='right')
  x$dBlatt <- rollapply(x$TGBlatt,  width = 2, FUN = diff, fill=NA, align='right')
  x$dKnolle <- rollapply(x$TGKnolle,  width = 2, FUN = diff, fill=NA, align='right')
  
  x$Rate_gesamt <- x$dGesamt/x$dDate
  x$Rate_blatt <- x$dBlatt/x$dDate
  x$Rate_knolle <- x$dKnolle/x$dDate
  ##########################################################################################
  ### Partitionierung
  x$Par_Blatt <- x$dBlatt/x$dGesamt
  x$Par_Knolle <- x$dKnolle/x$dGesamt
  x
}))

## ANOVA
fit <- aov(lm(TGGesamt~Variante*DAT, data=Data_all))
summary(fit)
fit2 <- aov(lm(Par_Blatt~Variante*DAT, data=Data_all))
summary(fit2)

fit3 <- lm(Par_Blatt~Variante*DAT, data=Data_all)
summary(fit3)

## Visualization
library(agricolae)
m1 <- aov(lm(Rate_gesamt~Variante, data=subset(Data_all, DAT==21)))
##Multiple comparisons: Tukey
out <- HSD.test(m1, "Variante", alpha=0.05, group=TRUE)
plot(out)

# Least significant difference
out2 <- LSD.test(m1, "Variante",console=TRUE)
plot(out2)


## Mittewert
Data_av <- aggregate(Data_all[,9:16], by=Data_all[,c("Datum","Variante", "DAT")], function(x){mean(na.omit(x))})
## Übung: Standardabweichung

#### Kritische Frage: Wann berechne ich Mittelwert für die weitere Berechnung?


###################  Ergebnisse in .csv Format speichern
write.table(Data_av, file = "test.csv", sep=";",row.names = FALSE)

