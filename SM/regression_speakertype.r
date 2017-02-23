#recommended version: R 3.3.2. Requires lme4, sjPlot and sjMisc for additional plots

ver = 254 #must be the same as in the analyzer.rb
measure = "expr" #choose measure 
library(lme4)

measure = "dexpr_noun_lex" 

#read data
epsilon_n <- read.csv(paste("v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
epsilon_i <- read.csv(paste("v",ver,"_",measure,"_i.csv",sep=""),header=T,sep=";",dec=".")
epsilon_d <- read.csv(paste("v",ver,"_",measure,"_d.csv",sep=""),header=T,sep=";",dec=".")

#set learner types
epsilon_n$type="l1"
epsilon_i$type="l1"
epsilon_d$type="l1"
epsilon_i[epsilon_i$generation==2 | epsilon_i$generation==3 | epsilon_i$generation==4,]$type="_l2"
epsilon_d[epsilon_d$generation>1,]$type="_l2"

#merge into single dataset
epsilon_nouns <- rbind(epsilon_n,epsilon_i,epsilon_d)
epsilon_nouns$meancat <- "noun"

measure = "dexpr_verb_lex" 

#read data
epsilon_n <- read.csv(paste("v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
epsilon_i <- read.csv(paste("v",ver,"_",measure,"_i.csv",sep=""),header=T,sep=";",dec=".")
epsilon_d <- read.csv(paste("v",ver,"_",measure,"_d.csv",sep=""),header=T,sep=";",dec=".")

#set learner types
epsilon_n$type="l1"
epsilon_i$type="l1"
epsilon_d$type="l1"
epsilon_i[epsilon_i$generation==2 | epsilon_i$generation==3 | epsilon_i$generation==4,]$type="_l2"
epsilon_d[epsilon_d$generation>1,]$type="_l2"

#merge into single dataset
epsilon_verbs <- rbind(epsilon_n,epsilon_i,epsilon_d)
epsilon_verbs$meancat <- "verb"

measure = "dexpr_noun_number" 

#read data
epsilon_n <- read.csv(paste("v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
epsilon_i <- read.csv(paste("v",ver,"_",measure,"_i.csv",sep=""),header=T,sep=";",dec=".")
epsilon_d <- read.csv(paste("v",ver,"_",measure,"_d.csv",sep=""),header=T,sep=";",dec=".")

#set learner types
epsilon_n$type="l1"
epsilon_i$type="l1"
epsilon_d$type="l1"
epsilon_i[epsilon_i$generation==2 | epsilon_i$generation==3 | epsilon_i$generation==4,]$type="_l2"
epsilon_d[epsilon_d$generation>1,]$type="_l2"

#merge into single dataset
epsilon_number <- rbind(epsilon_n,epsilon_i,epsilon_d)
epsilon_number$meancat <- "number"

measure = "dexpr_verb_gender" 

#read data
epsilon_n <- read.csv(paste("v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
epsilon_i <- read.csv(paste("v",ver,"_",measure,"_i.csv",sep=""),header=T,sep=";",dec=".")
epsilon_d <- read.csv(paste("v",ver,"_",measure,"_d.csv",sep=""),header=T,sep=";",dec=".")

#set learner types
epsilon_n$type="l1"
epsilon_i$type="l1"
epsilon_d$type="l1"
epsilon_i[epsilon_i$generation==2 | epsilon_i$generation==3 | epsilon_i$generation==4,]$type="_l2"
epsilon_d[epsilon_d$generation>1,]$type="_l2"

#merge into single dataset
epsilon_gender <- rbind(epsilon_n,epsilon_i,epsilon_d)
epsilon_gender$meancat <- "agreement"

epsilon <- rbind(epsilon_nouns,epsilon_verbs,epsilon_number,epsilon_gender)

#transform variables
epsilon$mvalue_c <- ((epsilon$mvalue)) #no change
epsilon$lang <- as.factor(epsilon$chain %% 15) #the random effect will be language (1-15), not chain (1-45)
epsilon$meancat <- as.factor(epsilon$meancat)
epsilon$type <- as.factor(epsilon$type)

dataset = epsilon


#regr0 <- lmer(mvalue_c ~ meancat + (1 + meancat|lang), data = dataset, REML = FALSE) #A proposal for the null model

#do we need the random slope?
regr0t <- lmer(mvalue_c ~ meancat + (1 + meancat|lang), data = dataset, REML = TRUE)
regr00t <- lmer(mvalue_c ~ meancat + (1|lang), data = dataset, REML = TRUE)
print(anova(regr0t,regr00t))
#No, the model is better without it

#Do we need the random slope for type?
regr00tt <- lmer(mvalue_c ~ meancat + (1+type|lang), data = dataset, REML = TRUE)
print(anova(regr00t,regr00tt))
#no

#Do we need the fixed effect?
regr0 <- lmer(mvalue_c ~ meancat + (1|lang), data = dataset, REML = FALSE)
regr00 <- lmer(mvalue_c ~ (1|lang), data = dataset, REML = FALSE)
print(anova(regr0,regr00))
#yes

#adding type as a fixed effect
regr1 <- lmer(mvalue_c ~ meancat + type +  (1|lang), data = dataset, REML = FALSE)
print(anova(regr0,regr1))

#adding interaction
regr2 <- lmer(mvalue_c ~ meancat * type +  (1|lang), data = dataset, REML = FALSE)
print(anova(regr2,regr1))

bestmodel = regr2

print(summary(bestmodel))
print(coef(bestmodel))

#test assumptions
plot(fitted(bestmodel),residuals(bestmodel)) #heteroskedasticity and linearity
dev.new()
#no obvious non-linear or heteroskedastic patterns

hist(residuals(bestmodel)) #normality
dev.new()
qqnorm(residuals(bestmodel)) #normality
dev.new()
print(shapiro.test(residuals(bestmodel)))
#the residuals are not normally distributed. However, the linear models are generally believed to be partly robust to that, especially with relatively big samples

library(sjPlot)
library(sjmisc)

sjp.lmer(bestmodel,type="pred",vars=c("type"),facet=FALSE) #Predicted values

#Do we have influential data points? (Takes quite a long time to compute)
coefs = summary(bestmodel)$coefficients[,1] #original coefficients
maxdiffs = numeric(length(coefs))
names(maxdiffs) = names(coefs)
for (i in 1:nrow(epsilon)) {
  epsilon2 = epsilon[-i,] #exclude a row (=given chain, given generation)
  newmodel = lmer(mvalue_c ~ meancat * type +  (1|lang), data = epsilon2, REML = FALSE)
  coefs2 = summary(newmodel)$coefficients[,1] #new coefficients
  if (!is.null(newmodel@optinfo$conv$lme4$code)) {
	  print(paste("The model does not converge without the row ",i))
  }
  for (j in 1:length(coefs)){
    if (sign(coefs[j])!=sign(coefs2[j])) {
	  print(paste("Influential data point in row ",i," The coefficient for ", names(coefs)[j], "changed sign."))
	}
	if (abs(coefs2[j]-coefs[j])>abs(maxdiffs[j])) {
	  maxdiffs[j] = coefs2[j]-coefs[j]
	}
  }
}

#No issues. The maximum differences are as follows:
print(maxdiffs)
#on the relative scale:
print(maxdiffs/coefs)
#max change ~9%