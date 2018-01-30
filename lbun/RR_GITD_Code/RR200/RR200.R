library(plyr)
library(ggplot2)
library(car)
library(foreign)
library(xlsx)
options(scipen=10000)
options(digits=4)

rm(list = ls())

RR <- read.dta("../RR-200-processed.dta")

## Brief 200 years analysis
RR <- subset(RR,(Year>=1791 & Country=="US")
             | (Year>=1830 & Country=="UK")
             | (Year>=1880 & Country=="Sweden")
             | (Year>=1850 & Country=="Spain")
             | (Year>=1880 & Country=="Portugal")
             | (Year>=1880 & Country=="Norway")
             | (Year>=1932 & Country=="New Zealand")
             | (Year>=1880 & Country=="Netherlands")
             | (Year>=1885 & Country=="Japan")
             | (Year>=1880 & Country=="Italy")             
             | (Year>=1949 & Country=="Ireland")                          
             | (Year>=1885 & Country=="Japan")
             | (Year>=1884 & Country=="Greece")
             | (Year>=1880 & Country=="Germany")             
             | (Year>=1880 & Country=="France")
             | (Year>=1914 & Country=="Finland")
             | (Year>=1880 & Country=="Denmark")
             | (Year>=1925 & Country=="Canada")
             | (Year>=1835 & Country=="Belgium")
             | (Year>=1880 & Country=="Austria")
             | (Year>=1902 & Country=="Australia")
             )


## Limit to actually available data
RR <- subset(RR,  !is.na(dRGDP) & !is.na(debtgdp))

## Actually available data years 
avail.data <- ddply(RR, ~Country, summarize, min.year=min(Year), count.year=sum(!is.na(dRGDP) & !is.na(debtgdp)))
avail.data[order(avail.data[,"min.year"]),]

## Create RR public debt/GDP categories
RR$dgcat.lm <- cut(RR$debtgdp, breaks=c(0,30,60,90,Inf))
RR$dgcat <- factor(RR$dgcat.lm, labels = c("0-30%","30-60%","60-90%","Above 90%"),ordered=TRUE)

## Create expanded public debt/GDP categories
RR$dgcat2.lm <- cut(RR$debtgdp, breaks=c(0,30,60,90,120,Inf))
RR$dgcat2 <- factor(RR$dgcat2.lm, labels = c("0-30%","30-60%","60-90%","90-120%","Above 120%"),ordered=TRUE)


(RR.equalwt.mean <- with(RR, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))
summary(RR.equalwt.mean)
## NYT Appendix Table 2 Line 2
(RR.correct.mean <- with(RR, tapply( dRGDP, dgcat, mean, na.rm=TRUE )))
RR.correct.mean.df <- data.frame(RR.correct.mean, dgcat=names(RR.correct.mean) )


## country weights
apply(RR.equalwt.mean,2,mean,na.rm=TRUE)
apply(RR.equalwt.mean,2,sd,na.rm=TRUE)
with(RR, table(Country,dgcat))
apply(with(RR,table( Country,dgcat)),2,sum)


## Test effect of spreadsheet error in 200-year analysis
RR.spreadsheet <- subset(RR, ! Country %in% c("Australia","Austria","Belgium","Canada","Denmark") )
(RR.spreadsheet.mean <- with(RR.spreadsheet, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))
summary(RR.spreadsheet.mean)
with(RR.spreadsheet, tapply( dRGDP, dgcat, mean, na.rm=TRUE ))

## Expanded categories 
(RR.ex.equalwt.mean <- with(RR, tapply( dRGDP, list(Country,dgcat2), mean, na.rm=TRUE )))
summary(RR.ex.equalwt.mean)
## NYT Appendix Table 3 
(RR.ex.correct.mean <- with(RR, tapply( dRGDP, dgcat2, mean, na.rm=TRUE )))
RR.ex.correct.mean.df <- data.frame(RR.ex.correct.mean, dgcat=names(RR.ex.correct.mean) )


## Compute standard errors
(mean200 <- with(RR, tapply( dRGDP, dgcat2, mean, na.rm=TRUE )))
(sd200 <- with(RR, tapply( dRGDP, dgcat2, sd, na.rm=TRUE )))
(length200 <- with(RR, tapply( dRGDP, dgcat2, length )))
sd200 / sqrt(length200)


L <- ggplot(RR, aes(x=dgcat2,y=dRGDP)) + geom_point(shape=3,color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Category")
## L <- L + geom_text(mapping=aes(label=Country ), size=2, hjust=-0.7,color='darkgray') 
L <- L + geom_point(RR.ex.correct.mean.df,  mapping=aes(x=dgcat,y=RR.ex.correct.mean,label=RR.ex.correct.mean), shape=16, size=4 )  + theme_bw()
L <- L + geom_text(RR.ex.correct.mean.df,  mapping=aes(x=dgcat,y=RR.ex.correct.mean,label=round(RR.ex.correct.mean,1)), hjust=1.7,size=3,color='darkgray')
print(L)


## Scatterplot (continuous)
library(mgcv)
RR.gam <- gam(dRGDP ~ s(debtgdp, bs="cs"),data=RR)

## Cross-validation technique for loess parameters
## http://stats.stackexchange.com/questions/2002/how-do-i-decide-what-span-to-use-in-loess-regression-in-r
m <- ggplot(RR, aes(x=debtgdp,y=dRGDP))
m1 <- m + geom_vline(xintercept=90,color='lightgray',size=1.5)
m1 <- m1 + geom_point(color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Ratio") + scale_x_continuous(breaks=seq(0,240,30)) + theme_bw()
## m1 <- m1 + geom_smooth(method='loess',span=1.0,color='black') + geom_smooth(method='loess',span=0.2,color='black')
m1 <- m1 + geom_smooth(method=gam, color='black',formula= y ~ s(x, bs = "cs"))
## m1 <- m1 + geom_smooth(method='auto', color='black')
print(m1)

## Scatterplot closeup
pdf("closeup.pdf",height=4,width=7)
m2 <- m + geom_point(color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Ratio") + scale_x_continuous(breaks=seq(0,240,30)) + theme_bw() +  geom_vline(xintercept=90,color='lightgray',size=1.5)
## m2 <- m2 + geom_smooth(method='loess',span=0.75,color='black') + geom_smooth(method='loess',span=0.4,color='black') 
## m2 <- m2 + geom_smooth(method='auto',color='black')
m2 <- m2 + geom_smooth(method=gam, color='black',formula= y ~ s(x, bs = "cs"))
m2 <- m2 + coord_cartesian(ylim=c(0, 7),xlim=c(0,150)) + scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7)) + theme_bw()
print(m2)

