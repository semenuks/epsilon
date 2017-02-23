#Recommended R version: 3.3.2
#required packages: pwr, effsize

library(pwr)
library(effsize)
ver = 254 #must be the same as in the analyzer.rb
measure = "change_rate" #select measure, see analyzer.rb or file names in Analyzed for codes
startgen = 0 
if (measure=="change_rate" | measure == "comprehension_rate")
  startgen = 1

#normal
t1 = read.csv(paste("v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
print(t.test(t1[t1$generation==startgen,]$mvalue,t1[t1$generation==10,]$mvalue,paired=TRUE))
print(cohen.d(t1[t1$generation==startgen,]$mvalue,t1[t1$generation==10,]$mvalue,paired=TRUE))
print(pwr.t.test(15,cohen.d(t1[t1$generation==startgen,]$mvalue,t1[t1$generation==10,]$mvalue,paired=TRUE)$estimate,type="paired"))

#temporarily interrupted
t2 = read.csv(paste("v",ver,"_",measure,"_i.csv",sep=""),header=T,sep=";",dec=".")
print(t.test(t2[t2$generation==startgen,]$mvalue,t2[t2$generation==10,]$mvalue,paired=TRUE))
print(cohen.d(t2[t2$generation==startgen,]$mvalue,t2[t2$generation==10,]$mvalue,paired=TRUE))
print(pwr.t.test(15,cohen.d(t2[t2$generation==startgen,]$mvalue,t2[t2$generation==10,]$mvalue,paired=TRUE)$estimate,type="paired"))

#permanently interrupted
t3 = read.csv(paste("v",ver,"_",measure,"_d.csv",sep=""),header=T,sep=";",dec=".")
print(t.test(t3[t3$generation==startgen,]$mvalue,t3[t3$generation==10,]$mvalue,paired=TRUE))
print(cohen.d(t3[t3$generation==startgen,]$mvalue,t3[t3$generation==10,]$mvalue,paired=TRUE))
print(pwr.t.test(15,cohen.d(t3[t3$generation==startgen,]$mvalue,t3[t3$generation==10,]$mvalue,paired=TRUE)$estimate,type="paired"))