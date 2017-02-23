#recommended version: R 3.3.2. Requires lme4, sjPlot and sjMisc for additional plots

ver = 254 #legacy variable. Has to be the same as in analyzer.rb
measure = "ttr" 
library ("lme4")
library ("influence.ME")

#read data
epsilon_n <- read.csv(paste("v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
epsilon_i <- read.csv(paste("v",ver,"_",measure,"_i.csv",sep=""),header=T,sep=";",dec=".")
epsilon_d <- read.csv(paste("v",ver,"_",measure,"_d.csv",sep=""),header=T,sep=";",dec=".")

#mark conditions
epsilon_n$condition = "n" #normal 
epsilon_i$condition = "t" #temp. interrupted
epsilon_d$condition = "_p" #perm. interrupted
epsilon <- rbind(epsilon_n,epsilon_i,epsilon_d)

#transform variables
#epsilon$mvalue <- as.numeric((epsilon$mvalue))
epsilon$generation_c <- as.numeric(scale(epsilon$generation),scale=FALSE) #center 
epsilon$lang <- as.factor(epsilon$chain %% 15) #the random effect will be language (1-15), not chain (1-45)
epsilon$condition <- as.factor(epsilon$condition)

#a proposal for the null model
#regr0 <- lmer(mvalue ~ generation_c + (1+generation_c|lang), data = epsilon, REML = FALSE)

#do we need the random slope?
regr0t <- lmer(mvalue ~ generation_c + (1+generation_c|lang), data = epsilon, REML = TRUE) #the null model
regr0tng <- lmer(mvalue ~ generation_c + (1|lang), data = epsilon, REML = TRUE) #the null model
print(anova(regr0t,regr0tng))
#yes

#do we need the correlation coefficient?
regr0tnc <- lmer(mvalue ~ generation_c + (0+generation_c|lang) + (1|lang), data = epsilon, REML = TRUE)
print(anova(regr0t,regr0tnc))
#yes

#do we need the random slope for condition?
regr0tc <- lmer(mvalue ~ generation_c + (1+generation_c|lang) + (1+condition|lang), data = epsilon, REML = TRUE) #does not converge
#what if we remove the correlation coefficient?
regr0tc <- lmer(mvalue ~ generation_c + (1+generation_c|lang) + (0+condition|lang) + (1|lang), data = epsilon, REML = TRUE) 
#nearly unindentifiable. We won't be adding the slope for condition

#do we need the fixed effect for generation?
#the null model
regr0 <- lmer(mvalue ~ generation_c + (1+generation_c|lang), data = epsilon, REML = FALSE)
#the extra-null model
regr00 <- lmer(mvalue ~ (1+generation_c|lang), data = epsilon, REML = FALSE)
print(anova(regr0,regr00))
#Yes. The null model stays at it is


#adding condition as a fixed effect
regr1 <- lmer(mvalue ~ condition + generation_c + (1+generation_c|lang), data = epsilon, REML = FALSE)
print(anova(regr0,regr1)) #regr1 is significantly better

#do we need the interaction?
regr2 <- lmer(mvalue ~ condition * generation_c + (1+generation_c|lang), data = epsilon, REML = FALSE)
print(anova(regr2,regr1)) 
#yes

bestmodel = regr2
print(summary(bestmodel))
print(coef(bestmodel))

#test assumptions
plot(fitted(bestmodel),residuals(bestmodel)) #heteroskedasticity and linearity
dev.new()
hist(residuals(bestmodel)) #normality
dev.new()
#no obvious non-linear or heteroskedastic patterns

qqnorm(residuals(bestmodel)) #normality
dev.new()
print(shapiro.test(residuals(bestmodel)))
#the residuals are not normally distributed. However, the linear models are generally believed to be partly robust to that, especially with relatively big samples

#additional plots
library(sjPlot)
library(sjmisc)
sjp.lmer(bestmodel,type="pred",vars=c("generation_c","condition"),facet=FALSE) #Predicted values
dev.new()
sjp.lmer(bestmodel,type="rs.ri") #Random slopes

#Do we have influential data points? (Takes quite a long time to compute)
coefs = summary(bestmodel)$coefficients[,1] #original coefficients
maxdiffs = numeric(length(coefs))
names(maxdiffs) = names(coefs)
for (i in 1:nrow(epsilon)) {
  epsilon2 = epsilon[-i,] #exclude a row (=given chain, given generation)
  newmodel = lmer(mvalue ~ condition * generation_c + (1+generation_c|lang), data = epsilon2, REML = FALSE)
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

#The model does not converge without N4-5 and N12-4. There are no cases when coefficients change sign. The maximum differences are as follows:
print(maxdiffs)
#on the relative scale:
print(maxdiffs/coefs)
#max change ~15%