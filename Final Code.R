happydata <- read.csv("World Happiness Exercise CSV.csv",header=T)
#
par(mfrow=c(2,2))

##General Data Examination from the perspective of the score.##
plot(happydata$Happiness.Score,happydata$Economy..GDP.per.Capita.,main="Happiness vs. GDP",xlab="Happiness Score",ylab="Economy (GDP Per Capita)")
#
plot(happydata$Happiness.Score,happydata$Family,main="Happiness vs. Family",xlab="Happiness Score",ylab="Family")
#
plot(happydata$Happiness.Score,happydata$Health..Life.Expectancy.,main="Happiness vs. Health as Life Expectancy",xlab="Happiness Score",ylab="Health as Life Expectancy")
#
plot(happydata$Happiness.Score,happydata$Freedom,main="Happiness vs. Freedom",xlab="Happiness Score",ylab="Freedom")
#
plot(happydata$Happiness.Score,happydata$Trust..Government.Corruption.,main="Happiness vs. Trust in Government",xlab="Happiness Score",ylab="Government Trust")
#
plot(happydata$Happiness.Score,happydata$Generosity,main="Happiness vs. Generosity",xlab="Happiness Score",ylab="Generosity")
#
plot(happydata$Happiness.Score,happydata$Dystopia.Residual,main="Happiness vs. Dystopia Residual",xlab="Happiness Score",ylab="Dystopia Residual")
#
##Creating a single variable class matrix for the overall view.##
install.packages("dplyr")
library(dplyr)
HappyData <- select(happydata,Region,Happiness.Score,Economy..GDP.per.Capita.,Family,Health..Life.Expectancy.,Freedom,Trust..Government.Corruption.,Generosity)
##Numerically evaluating the significance of each variable against the score##
fit.gdp <- lm(Happiness.Score~Economy..GDP.per.Capita.,data=happydata)
summary(fit.gdp)
#
fit.family <- lm(Happiness.Score~Family,data=happydata)
summary(fit.family)
#
fit.health <- lm(Happiness.Score~Health..Life.Expectancy.,data=happydata)
summary(fit.health)
#
fit.freedom <- lm(Happiness.Score~Freedom,data=happydata)
summary(fit.freedom)
#
fit.trust <- lm(Happiness.Score~Trust..Government.Corruption.,data=happydata)
summary(fit.trust)
#
fit.generous <- lm(Happiness.Score~Generosity,data=happydata)
summary(fit.generous)
#
fit.dystop <- lm(Happiness.Score~Dystopia.Residual,data=happydata)
summary(fit.dystop)
#
fit.all <- lm(Happiness.Score~.,data=HappyData)
summary(fit.all)
##Summary verifies the greater magnitude of significance of various factors.##
plot(fit.all)

## Removing missing values/zeros for accuarate range value ##
happydatatester <- happydata
happydatatester[happydatatester == 0] <- NA
#
diff(range(happydatatester$Economy..GDP.per.Capita.,na.rm=T))
#
diff(range(happydatatester$Family,na.rm=T))
#
diff(range(happydatatester$Health..Life.Expectancy.,na.rm=T))
#
diff(range(happydatatester$Freedom,na.rm=T))
#
diff(range(happydatatester$Trust..Government.Corruption.,na.rm=T))
#
diff(range(happydatatester$Generosity,na.rm=T))
#
diff(range(happydatatester$Dystopia.Residual,na.rm=T))

## Comparing the three highest variables to see if any variable was a strong predictor##
fit.health.gdp <- lm(Health..Life.Expectancy.~Economy..GDP.per.Capita.,data=happydata)
summary(fit.health.gdp)
plot(fit.health.gdp)
#
fit.gdp.family <- lm(Economy..GDP.per.Capita.~Family,data=happydata)
summary(fit.gdp.family)
plot(fit.gdp.family)
#
fit.family.health <- lm(Family~Health..Life.Expectancy.,data=happydata)
summary(fit.family.health)
plot(fit.family.health)