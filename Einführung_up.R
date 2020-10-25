

################### 4. Wo sind die Daten gespeichert?########################################
##Import raw data as a dataframe (table) named df
setwd("~/_Lehren/R_lehren")


################### 5. Funktion von Tab 

###################  Was ist ein Objekt und seine Eigenschaft?
## Ein Objekt x, solche Objekte könnte man al globale Parameter definieren.
x <- 1
x1 <- "1"
y <- 0:100

###################  Was ist eine Funktion?
## Mittelwert von y
mean(y)
## Standardabweichung von y
sd(y)
## Summe von y
sum(y)

###################  Was ist ein Packet?, Ein Packet beinhaltet/definiert eine oder mehre Funktionen, die i
## Packet agricolae laden.
library(agricolae)


###################  Die Daten einlesen/importieren (read.table(); read.csv() etc.)
## reading the data
df <- read.table("Daten_Radieschen_2004.csv",sep=";",dec=".",h=T)
klima <- read.csv("Klima.csv",sep=";",dec=".",h=T)


###################Die Struktur der Daten prüfen (int, num, chr, Factor, Date.) und ändern
## Namen der Spalte

str(df)
## The first rows of the data
head(df)
## example: as.Date, the part "%Y-%m-%d" is adapted by the form of the data
df$Datum <- as.Date(df$Datum, "%Y-%m-%d")
df$Variante <- as.factor(paste("W", df$Variante, sep=""))


###################Was ist ein "data frame"
# Neue Spalte
# 
# Rechnen
df$TGGesamt <- df$TGBlatt + df$TGKnolle
DOT <- as.Date("2004-11-04")
df$DAT <- df$Datum-DOT
## Klima
T0 <- 3.5  ## mit der Annahme dass Basistemperatur 0.5°C ist.
klima$eTemp <- klima$Temperature-T0
## cumulative Summe rechnen:
klima$TS <- cumsum(klima$eTemp)
#Vorsicht mit überschreiben!

###################Hilfe!! Hilfe!!

# Googeln
# ? in Console
## ?? in Console

###################  Wie mache ich eine Abbildung in R?
plot(TGGesamt~DAT, data=df)
## mit Farbe col=
plot(TGGesamt~DAT, col=Variante, data=df)
## Überschrift
plot(TGGesamt~DAT, col=Variante, data=df, main="Ich bin Überschrift")
## Beschriftung
plot(TGGesamt~DAT, col=Variante, data=df, main="Ich bin Überschrift", xlab="Ich bin x")
## Export-Abbildung

## ANOVA mit einem Faktor
fit1 <- aov(TGGesamt~Variante, data=df)
summary(fit1)
## ANOVA mit zwei Faktoren und Wechselwirkung
fit2 <- aov(TGGesamt~Variante+DAT+Variante:DAT, data=df)
summary(fit2)

## lineare Regression
model1 <- lm(TGGesamt~DAT, data=df)
summary(model1)

## Konfidenzinterval
## Wachstumsrate
## nichtlineare Regression
## abline
#y=a*exp(b*x)
# df$DAT <- as.numeric(df$DAT)
#test <- nls(TGGesamt~a*exp(b*DAT), data=df, start = list(a = 0.5, b = 0.2))
#abline(test)

## Plotting mit Temperatursumme
klima$Date <- as.Date(klima$Date, "%d.%m.%Y")
library(plyr)
klima <- rename(klima, c("Date"="Datum"))
df2 <- merge(df, klima, by = "Datum")


########### Subsetting data
df[1, 1]

## using "subset" to select the part of the data 
test <- subset(df, DAT>20)
## quick select
##Create test with the 3rd, 5th columns of df 
test <- df[,c(3,5)]
test <- df[df$Variante =="W100",]
test <- df[df$Variante =="W100"  & df$Wiederholung >=2,]

###### using loop to do routine works
for(i in xxx)
{
  "was gemacht werden muss für i in xxx"
  
}


test <- colnames(df[,c(4:6)])
for (i in test)
{
  ## ANOV2 <- aov(i~Variante+DAT, data=df[,c("DAT", "Variante", i)])  ## funktioniert nicht....
  ANOV2 <- aov(as.formula(paste(i, "~Variante+DAT")), data=df[,c("DAT", "Variante", i)])
  summary(ANOV2)
  out2 <- HSD.test(ANOV2, c("Variante", "DAT"), group=TRUE)
  plot(out2, main=i)
  
}

############ Pause!######################################################################################
## round the number 
x <- round(Plant_distance, digits=5)

## rename columns of the dataframe
library(plyr)
df <- rename(df, c("v1"="v1_new", "v2"="v2_new"))

## subset data
df2 <- droplevels(df[df$T1 > 45 & df$T2 < 2 & df$Treatment=="T1",])
## create a dataframe df2 by subsetting dataframe df 
df2 <- subset(df, Treatment=="T1")



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



## Vorsicht! Write data
## write.table(Data, file = "BRIWECS_Data_all.csv", sep=";",row.names = FALSE)
