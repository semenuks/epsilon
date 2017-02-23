#recommended version: R 3.3.2. Requires lme4, sjPlot and sjMisc for additional plots

ver = 254 #must be the same as in the analyzer.rb
measure = "expr" #choose measure
library(lme4)

#reading and merging data
epsilon_n <- read.csv(paste("v",ver,"_",measure,"_noun_number_n.csv",sep=""),header=T,sep=";",dec=".")
epsilon_i <- read.csv(paste("v",ver,"_",measure,"_noun_number_i.csv",sep=""),header=T,sep=";",dec=".")
epsilon_d <- read.csv(paste("v",ver,"_",measure,"_noun_number_d.csv",sep=""),header=T,sep=";",dec=".")
epsilon_n$condition="n"
epsilon_i$condition="t"
epsilon_d$condition="_p"
epsilon_nouns <- rbind(epsilon_n,epsilon_i,epsilon_d)
epsilon_nouns$meancat <- "number"

epsilon_n <- read.csv(paste("v",ver,"_",measure,"_verb_gender_n.csv",sep=""),header=T,sep=";",dec=".")
epsilon_i <- read.csv(paste("v",ver,"_", measure,"_verb_gender_i.csv",sep=""),header=T,sep=";",dec=".")
epsilon_d <- read.csv(paste("v",ver,"_",measure,"_verb_gender_d.csv",sep=""),header=T,sep=";",dec=".")
epsilon_n$condition="n"
epsilon_i$condition="t"
epsilon_d$condition="_p"
epsilon_verbs <- rbind(epsilon_n,epsilon_i,epsilon_d)
epsilon_verbs$meancat <- "agreement"

epsilon_n <- read.csv(paste("v",ver,"_",measure,"_verb_lex_n.csv",sep=""),header=T,sep=";",dec=".")
epsilon_i <- read.csv(paste("v",ver,"_",measure,"_verb_lex_i.csv",sep=""),header=T,sep=";",dec=".")
epsilon_d <- read.csv(paste("v",ver,"_",measure,"_verb_lex_d.csv",sep=""),header=T,sep=";",dec=".")
epsilon_n$condition="n"
epsilon_i$condition="t"
epsilon_d$condition="_p"
epsilon_lverbs <- rbind(epsilon_n,epsilon_i,epsilon_d)
epsilon_lverbs$meancat <- "verbstem"

epsilon_n <- read.csv(paste("v",ver,"_",measure,"_noun_lex_n.csv",sep=""),header=T,sep=";",dec=".")
epsilon_i <- read.csv(paste("v",ver,"_",measure,"_noun_lex_i.csv",sep=""),header=T,sep=";",dec=".")
epsilon_d <- read.csv(paste("v",ver,"_",measure,"_noun_lex_d.csv",sep=""),header=T,sep=";",dec=".")
epsilon_n$condition="n"
epsilon_i$condition="t"
epsilon_d$condition="_p"
epsilon_lnouns <- rbind(epsilon_n,epsilon_i,epsilon_d)
epsilon_lnouns$meancat <- "nounstem"

epsilon <- rbind(epsilon_nouns,epsilon_verbs,epsilon_lverbs,epsilon_lnouns)

#transform variables
epsilon$mvalue_c <- epsilon$mvalue #no change
epsilon$generation_c <-  as.numeric(scale(epsilon$generation,scale=FALSE)) #center
epsilon$lang <- as.factor(epsilon$chain %% 15) #the random effect will be language (1-15), not chain (1-45)
epsilon$condition <- as.factor(epsilon$condition)
epsilon$meancat <- as.factor(epsilon$meancat)

#regr0 <- lmer(mvalue_c ~ generation_c + (1+generation_c|lang), data = epsilon, REML = FALSE) #a proposal for the null model
#do we need the random slope?
regr0t <- lmer(mvalue_c ~ generation_c + (1+generation_c|lang), data = epsilon, REML = TRUE) 
regr0tng <- lmer(mvalue_c ~ generation_c + (1|lang), data = epsilon, REML = TRUE) 
print(anova(regr0t,regr0tng))
#no
regr0t <- lmer(mvalue_c ~ generation_c + (1|lang), data = epsilon, REML = TRUE) 

#add random slope for meancat?
regr0tm <- lmer(mvalue_c ~ generation_c + (1+meancat|lang), data = epsilon, REML = TRUE) 
print(anova(regr0t,regr0tm))
#the slope is warranted
#add random slope for condition instead?
regr0tc <- lmer(mvalue_c ~ generation_c + (1+condition|lang), data = epsilon, REML = TRUE) 
print(anova(regr0t,regr0tc))
#the slope is warranted. Do we need both slopes?
regr0tmc <- lmer(mvalue_c ~ generation_c + (1+condition|lang) + (1+meancat|lang), data = epsilon, REML = TRUE) 
#does not converge. Removing correlation coefficients would make the model sensitive to the coding of the fixed effects (Barr et al. 2013), so we won't try that. Let's check the two converging models:
print(anova(regr0tm,regr0tc)) #the slope for meancat is significantly more important

regr0 <- lmer(mvalue_c ~ generation_c + (1+meancat|lang), data = epsilon, REML = FALSE) 
#do we need the fixed effect?
regr00 <- lmer(mvalue_c ~ (1+meancat|lang), data = epsilon, REML = FALSE) 
print(anova(regr0,regr00))
#yes

#adding condition as a fixed effect
regr1 <- lmer(mvalue_c ~ generation_c + condition + (1+meancat|lang), data = epsilon, REML = FALSE) 
print(anova(regr0,regr1))
#sign. better than regr0

#adding interaction
regr2 <- lmer(mvalue_c ~ generation_c * condition + (1+meancat|lang), data = epsilon, REML = FALSE) 
print(anova(regr2,regr1))
#the difference is significant. regr2 has lower AIC, regr1 has lower BIC. We are not aware of any standard solution for the criteria conflict and stick to AIC as the main criterion (reporting the issue)

#adding meancat
regr3 <- lmer(mvalue_c ~ meancat + generation + (1+meancat|lang) , data = epsilon, REML = FALSE) #does not converge
print(anova(regr2,regr3))
#no significant difference

#adding interaction
regr4 <- lmer(mvalue_c ~ meancat * generation + (1+meancat|lang), data = epsilon, REML = FALSE) #
print(anova(regr2,regr4))
print(anova(regr3,regr4))
#regr4 significantly better in both cases

#trying both fixed effects at once
regr5 <- lmer(mvalue_c ~ condition + meancat + generation_c + (1+meancat|lang), data = epsilon, REML = FALSE) 
print(anova(regr5,regr4))
#worse than regr4

#trying various combinations of interactions
regr6 <- lmer(mvalue_c ~ condition * meancat + generation_c + (1+meancat|lang), data = epsilon, REML = FALSE) 
print(anova(regr6,regr4))
#worse than regr4

regr7 <- lmer(mvalue_c ~ condition + meancat * generation_c + (1+meancat|lang), data = epsilon, REML = FALSE)
#does not converge

regr8 <- lmer(mvalue_c ~ condition * generation_c + meancat + (1+meancat|lang), data = epsilon, REML = FALSE)
print(anova(regr8,regr4))
#Despite large differences, there is no significance. Note, however, that regr4 can be argued to be simpler.

regr9 <- lmer(mvalue_c ~ condition * generation_c * meancat + (1+meancat|lang) , data = epsilon, REML = FALSE)
print(anova(regr9,regr4))
#regr9  is significantly better

bestmodel = regr9

#see detailed coefficients
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

#additional plots
library(sjPlot)
library(sjmisc)
sjp.lmer(bestmodel,type="rs.ri") #Random slopes
dev.new()
sjp.lmer(bestmodel,type="pred",vars=c("generation_c","condition"),facet=FALSE) #Predicted values
dev.new()
sjp.lmer(bestmodel,type="pred",vars=c("generation_c","meancat"),facet=FALSE) #Predicted values
dev.new()
sjp.lmer(bestmodel,type="pred",vars=c("meancat"),facet=FALSE) #Predicted values

#Do we have influential data points? (Takes quite a long time to compute)
coefs = summary(bestmodel)$coefficients[,1] #original coefficients
maxdiffs = numeric(length(coefs))
names(maxdiffs) = names(coefs)
for (i in 1:nrow(epsilon)) {
  epsilon2 = epsilon[-i,] #exclude a row (=given chain, given generation)
  newmodel = lmer(mvalue_c ~ condition * generation_c * meancat + (1+meancat|lang) , data = epsilon2, REML = FALSE)
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

#The model does not converge without N10-3, N10-4, N10-5, T26-9 and T26-10. The coefficient for conditiont often changes sign, but remember that is almost equal to zero (and always remains so). The maximum differences are as follows:
print(maxdiffs)
#on the relative scale:
print(maxdiffs/coefs)
#max change ~240% (for condition t, the coefficient remains very small), next 107% (for conditiont:meancatverbstem), same thing.