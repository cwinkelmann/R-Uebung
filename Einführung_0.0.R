

################### 2. Wo sind die Daten gespeichert?########################################
##Import raw data as a dataframe (table) named df
### setwd("~/_Lehren/R_lehren")

## zweite Möglichkeit
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(this.dir)


################### 3. Tab 

################### 4. Was ist ein Objekt und seine Eigenschaft?
x <- 1


#### Einfache Mathe ####

x*5

###################  5. Was ist eine Funktion?

### Mittelwert

### Standardabweichung

### Summe

### Beispiel


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



################### 9. Was ist ein "data frame"
################### 10. Datenbearbeitung in R
# Neue Spalte

# Rechnen


## Berechung der Thermalzeit und Temperatursumme

### 1. Lineares Modell mit Basistemperatur
## Basistemperatur
Tbase <- 3.5
## Berechnung der Thermalzeit °Cd

## Berechnung der Temperatursumme



###################  11. Wie mache ich eine Abbildung in R?
## Klimadaten plotten
plot(klima$Temperature~klima$Date)

## Überschrift
## Beschriftung
## Export-Abbildung
## lineare Regression
## Wachstumsrate
## nichtlineare Regression
## abline
## Konfidenzinterval


### Graphische Parameter in R
# https://www.statmethods.net/advgraphs/parameters.html
# R cheat sheet https://rstudio.com/resources/cheatsheets/
## googeln : r plot cheatsheet, Bilder

### Linie
abline(Topt, 0)
abline(Tmax, 0, col="red")
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
plot(y~x)

klima$Thermalzeit3 <- A*(klima$Temperature+K)*exp(-deltaH/(R*(klima$Temperature+K)))/(1+exp(-deltaH/(R*(klima$Temperature+K)))^(alpha*(1-(klima$Temperature+K)/T0)))
klima$TS3 <- cumsum(klima$Thermalzeit3)
## Checken
plot(klima$Thermalzeit3~klima$Thermalzeit1)

#Vorsicht mit Überschreiben!

###################Hilfe!! Hilfe!!

# Googeln
# ? in Console
## ?? in Console


df[1, 1]

## using "subset" to select the part of the data 
test <- subset(df, DAT>20)
## quick select
##Create test with the 3rd, 5th columns of df 



## ANOVA

#y=a*exp(b*x)
# df$DAT <- as.numeric(df$DAT)
#test <- nls(TGGesamt~a*exp(b*DAT), data=df, start = list(a = 0.5, b = 0.2))
#abline(test)

#fit <- aov(lm(TGGesamt~Variante, data=subset(df, DAT>25)))
#fit
#summary(fit)

## Visualization
library(agricolae)
##Multiple comparisons: Tukey
out <- HSD.test(fit, "Variante", group=TRUE)
plot(out, main="Wachstumsrate")
plot(out, main="Wachstumsrate", ylim=c(0,3))


out2 <- LSD.test(fit, "Variante",console=TRUE)
plot(out2)

#df$DAT <- as.factor(df$DAT)
#fit2 <- aov(TGGesamt~Variante+DAT+Variante:DAT, data=df)
#summary(fit2)


## The structure of the data
## using "split" to structure the data in small sets
#x <-split(df, df$Datum, drop=TRUE)[[1]]
#x <-split(df, list(df$Variante, df$Wiederholung), drop=TRUE)[[1]]


### Vergleich Wachstumsrate 
XXXXX = do.call('rbind',lapply(split(df, df$Variante, drop=TRUE), function(x) {
  # x <- split(df, df$Variante, drop=TRUE)[[1]]    ### Testing if thins works
  x
}))




## Wachstumsrate berechnen:
library(zoo)
x$dDate <- rollapply(x$Datum,  width = 2, FUN = diff.Date, fill=NA, align='right')
## difference in whole dry mass
x$dGesamt <- rollapply(x$TGGesamt,  width = 2, FUN = diff, fill=NA, align='right')
## difference in leaf dry mass
x$dBlatt <- rollapply(x$TGBlatt,  width = 2, FUN = diff, fill=NA, align='right')
## difference in bulb dry mass
x$dKnolle <- rollapply(x$TGKnolle,  width = 2, FUN = diff, fill=NA, align='right')
## whole plant growth rate
x$Gesamt_Rate <- x$dGesamt/x$dDate
x$Blatt_Rate <- x$dBlatt/x$dDate
x$Knolle_Rate <- x$dKnolle/x$dDate
x

## do.call

XXXXX = do.call('rbind',lapply(split(df, list(df$Variante, df$Wiederholung), drop=TRUE), function(x) {
  # x <- split(df, list(df$Variante, df$Wiederholung))[[1]]    ### Testing if thins works
   x
}))




###################  Ergebnisse in .csv Format speichern







set.seed(218123)
## Generate 6 values from 1 to 200 
for (i in 1:4){
  test <- data.frame(n =sample(1:6,6, replace=F))## replace = T means the value can be repeated
  x <- data.frame(Behandlung=c("NaCl_S0","NaCl_S1","NaCL_S2", "NaCl_S0","NaCl_S1","NaCL_S2"))
  ran <- cbind(test,x)
  ran <- ran[order(ran$n),]
  print(ran$Behandlung)
}


