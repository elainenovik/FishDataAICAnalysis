library(readxl)
library(BBmisc)
my_data<- read_excel("Mercury Final.xlsx")
View(my_data)
keeps<-c("Site", "Depth", "Length", "LogLength", 
         "Weight", "Maturity", "Morph", "Adjdel13C", 
         "del15N", "del34S","WetHg (ppm)")
newdata<- my_data[keeps]
names(newdata)[11]<- "WetHg"

newdata[,11] <- log(newdata[11])
names(newdata)[11]<-"LWetHg"

newdata[,10] <- log(newdata[10])
names(newdata)[10]<-"Ldel34S"

newdata[,9] <- log(newdata[9])
names(newdata)[9]<-"Ldel15N"

newdata[,5] <- log(normalize(newdata[5], method='range', range=c(0,1)))
names(newdata)[5]<-"LWeight"
hist(newdata$LWeight)

newdata<- subset(newdata, LWeight!="-Inf")
newdata<- subset(newdata, Maturity!="Unknown")
View(newdata)
attach(newdata)

library(dplyr)
aicdata<-select(newdata,-Site)
aicdata<-select(aicdata,-Length)
attach(aicdata)
View(aicdata)

library(MuMIn)
#AIC analysis
mod1 <- lm(LWetHg~Ldel15N, data = aicdata)
mod2 <- lm(LWetHg~LogLength + Depth, data = aicdata)
mod3 <- lm(LWetHg~Ldel15N + Ldel34S, data = aicdata)
mod4 <- lm(LWetHg~Ldel15N + Ldel34S + Depth, data = aicdata)
mod5 <- lm(LWetHg~Ldel15N + Ldel34S + Depth + LogLength, data = aicdata)
mod6 <- lm(LWetHg~Ldel15N + Ldel34S + Depth + LogLength + Adjdel13C, data=aicdata)
mod7 <- lm(LWetHg~Ldel15N + Ldel34S + Depth + LogLength + + Adjdel13C + Maturity + Morph, data=aicdata)

library(MuMIn)

allaic <- c(AIC(mod1), AIC(mod2), AIC(mod3), AIC(mod4), AIC(mod5), AIC(mod6), AIC(mod7))
allaic
delaic <- allaic - min(allaic)
likelihood <- exp(-0.5 * delaic)
aicweight <- likelihood/sum(likelihood)
eratio <- max(aicweight)/aicweight

aictable <- data.frame(AICc = allaic, 
                       deltaAIC = delaic,
                       ModelLikelihood = likelihood,
                       ModelWeight = aicweight,
                       EvidenceRatio = eratio)
rownames(aictable) <- c("Model 1", "Model 2","Model 3",
                        "Model 4","Model 5","Model 6","Model 7" )
round(aictable, digits = 3)
aictable
