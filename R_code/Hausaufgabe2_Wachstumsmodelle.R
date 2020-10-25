

data <- data.frame("Tag" = 1:14, "Temp" = c(21,24,23.5,26,28,31,26,18,22,25,24,29,32,25))
data$TZa <- data$Temp-3.5
data$TSa <- cumsum(data$TZa)

Tbase <- 3.5
Topt <- 21.6
Tmax <- 32.1

## Achtung!! Nur für diese Temperaturdaten gültig!
data$TZb <- ifelse(data$Temp<Topt, ## if
                             Topt*(data$Temp-Tbase)/(Topt-Tbase),            ## yes
                             Topt*(Tmax-data$Temp)/(Tmax-Topt)              ## no
)
data$TSb <- cumsum(data$TZb)


deltaH=63600
A=937088859.183916
alpha = 3.5
R = 8.314
K = 273
T0=297.4

data$TZc <- A*(data$Temp+K)*exp(-deltaH/(R*(data$Temp+K)))/(1+exp(-deltaH/(R*(data$Temp+K)))^(alpha*(1-(data$Temp+K)/T0)))
data$TSc <- cumsum(data$TZc)

plot(TSa~Tag, data=data, type="b", pch=17, col="blue", xlab="Tag", ylab="Temperatursumme", ylim=c(0,310), xlim=c(0,15))
lines(TSb~Tag, data=data,type="b", pch=19, col="red")
lines(TSc*20~Tag, data=data,type="b", pch=18, bg="darkgreen", col="darkgreen")
legend(1, 300, legend=c("TSa", "TSb", "TSc"),
       col=c("blue", "red", "darkgreen"), pch=c(17,19,18))

