#load packages
library ("lme4")
library("lmerTest")
ver = 362 #legacy variable. Has to be the same as in data_analyzer.rb

#which measures should be analyzed
ttr = TRUE
expressibility = FALSE
learnability = FALSE
learnability_as_function_of_expressibility = FALSE

if (ttr) {
#TTR ANALYSIS
measure = "ttr" 

#read data
epsilon_n <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
epsilon_t <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_t.csv",sep=""),header=T,sep=";",dec=".")
epsilon_p <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_p.csv",sep=""),header=T,sep=";",dec=".")

#mark conditions
epsilon_n$condition = "n" #normal 
epsilon_t$condition = "_t" #temp. interrupted, reference level
epsilon_p$condition = "p" #perm. interrupted

#mark learner types
epsilon_n$learnertype="l1" #l1 = normal learner
epsilon_t$learnertype="l1"
epsilon_p$learnertype="l1"
epsilon_t[epsilon_t$generation==2 | epsilon_t$generation==3 | epsilon_t$generation==4,]$learnertype="l2"
epsilon_p[epsilon_p$generation>1,]$learnertype="l2" #l2 = imperfect learner

#merge the datasets
epsilon <- rbind(epsilon_n,epsilon_t,epsilon_p)

#transform variables
epsilon$condition2 <- as.factor(epsilon$condition)
epsilon$chain2 <- as.factor(epsilon$chain)
epsilon$mvalue2 <- log(epsilon$mvalue) #log transformation to prevent the model from predicting negative TTR for generations after 10. We do not have to correct for non-zero values, since TTR > 0

#The maximal model
regrmax <- lmer(mvalue2 ~ generation*condition2 + (1+generation|chain2),data = epsilon, REML = TRUE)

bestmodel <- regrmax
print(summary(bestmodel))
print(confint(bestmodel,"beta_")) #profile confidence intervals

#random effect coefficients
print(coef(bestmodel))
 
#test assumptions
plot(fitted(bestmodel),residuals(bestmodel)) #No clear signs of heteroskedasticity and non-linearity. 
dev.new()
boxplot(residuals(bestmodel) ~ epsilon$condition) #Moderate heteroskedasticity (variance is lower for N than for T and P)
dev.new()

hist(residuals(bestmodel)) #testing for normality
dev.new()
qqnorm(residuals(bestmodel)) #testing for normality
dev.new()
print(shapiro.test(residuals(bestmodel)))
#the residuals are not normally distributed. Gelman and Hill (2007: 46) describe the assumption of the normality of residuals as "least important", and for the purpose of estimating the regression line even "barely important".

#Do we have influential data points? 
coefs = summary(bestmodel)$coefficients[,1] #original coefficients
coefs <- sort(coefs)
maxdiffs = numeric(length(coefs))
names(maxdiffs) = names(coefs)
nonconvmodels = 0
infl_points1 = 0
infl_points2 = 0

#exclude every single data point and test if there are significant changes in the coefficients
for (i in 1:nrow(epsilon)) {
  epsilon_excluded = epsilon[-i,] #exclude a row (=given chain, given generation)
  newmodel = lmer(mvalue2 ~  generation * condition2 + (1+generation|chain2), data = epsilon_excluded, REML = TRUE)
  coefs2 = summary(newmodel)$coefficients[,1] #new coefficients
  coefs2 <- sort(coefs2)
  if (!is.null(newmodel@optinfo$conv$lme4$code)) {
	  nonconvmodels = nonconvmodels + 1
  }
  for (j in 1:length(coefs)){
 if (names(coefs[j])=="generation" | names(coefs[j]) =="generation:conditionn") { #we are not interested in non-significant coefficients
      if (sign(coefs[j])!=sign(coefs2[j])) {
	      infl_points1 = infl_points1 + 1
      } else if (abs((coefs[j]-coefs2[j])/coefs2[j])>0.2){
	      infl_points2 = infl_points2 + 1
          }
	  }  
	  if (abs(coefs2[j]-coefs[j])>abs(maxdiffs[j])) {
	    maxdiffs[j] = coefs2[j]-coefs[j]
	  }
    }
  }
  
print("If we exclude datapoints one by one...")
print(paste("...there are ", nonconvmodels, " non-converging models" ))
print(paste("..there are ", infl_points1, " cases when a significant coefficient changes sign" ))
print(paste("There are ", infl_points2, " cases when the absolute relative change of a significant coefficient is larger than 0.2"))

#The maximum differences are as follows:
print(maxdiffs)
#on the relative scale:
print(maxdiffs/coefs)
}

if (expressibility) {
  measure = "expr_verb_gender" #it is also possible to analyze the expressibility of another category
  
  #read data 
  epsilon_n <- read.csv(paste(".\\Measures\\", "v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste(".\\Measures\\", "v",ver,"_",measure,"_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste(".\\Measures\\", "v",ver,"_",measure,"_p.csv",sep=""),header=T,sep=";",dec=".")
  
  epsilon_n$condition="n" #normal
  epsilon_t$condition="_t" #temp_interrupted, reference level
  epsilon_p$condition="p" #perm_interrupted 
  
  #merge the datasets
  epsilon2 <- rbind(epsilon_n,epsilon_t,epsilon_p)
  
  
  epsilon2$mvalue2 <- log(epsilon2$mvalue+1)  #log transformation to prevent the model from predicting negative TTR for generations after 10. We add 1 to correct for zero values, since expr can be equal to 0
  
  epsilon2$condition2 <- as.factor(epsilon2$condition)
  epsilon2$chain2 <- as.factor(epsilon2$chain) 
  epsilon2$generation2 <- log(epsilon2$generation+1) #log-transforming the generation, since the relationship does not seem to be linear. Adding 1 to correct for zero values
  
  #maximal model
  expr_regrmax <- lmer(mvalue2 ~ generation2 * condition2 + (1 + generation2 | chain2), data = epsilon2, REML = TRUE)
  print(confint(expr_regrmax,"beta_")) #profile confidence intervals
  #cannot be calculated. Let's remove the random correlation parameter, the first step recommended by Barr et al. 2013.
  expr_regrmax0 <- lmer(mvalue2 ~ generation2 * condition2 + (0 + generation2 | chain2) + (1 | chain), data = epsilon2, REML = TRUE)
  print(confint(expr_regrmax0,"beta_"))
  #OK
  #Is the simplified model worse?
  print(anova(expr_regrmax,expr_regrmax0))
  #No significant difference
  
  expr_bestmodel <- expr_regrmax0
  print(summary(expr_bestmodel))
  
  
  #random effect coefficients
  print(coef(expr_bestmodel))
    
  #test assumptions
  plot(fitted(expr_bestmodel),residuals(expr_bestmodel)) #No clear signs of heteroskedasticity and non-linearity. 
  dev.new()
  hist(residuals(expr_bestmodel)) #testing for normality
  dev.new()
  qqnorm(residuals(expr_bestmodel)) #testing for normality
  dev.new()
  print(shapiro.test(residuals(expr_bestmodel))) #the residuals are not normally distributed. Gelman and Hill (2007: 46) describe the assumption of the normality of residuals as "least important", and for the purpose of estimating the regression line even "barely important".

  boxplot(residuals(expr_bestmodel) ~ epsilon2$condition) #No clear signs of heteroskedasticity and non-linearity. 
  
  #Do we have influential data points? 
  coefs = summary(expr_bestmodel)$coefficients[,1] #original coefficients
  coefs <- sort(coefs)
  maxdiffs = numeric(length(coefs))
  names(maxdiffs) = names(coefs)
  nonconvmodels = 0
  infl_points1 = 0
  infl_points2 = 0
  
  #exclude every single data point and test if there are significant changes in the coefficients
  for (i in 1:nrow(epsilon2)) {
    epsilon_excluded = epsilon2[-i,] #exclude a row (=given chain, given generation)
    newmodel = lmer(mvalue2 ~ generation2 * condition2 + (1 + generation2|chain2), data = epsilon_excluded, REML = TRUE)
    coefs2 = summary(newmodel)$coefficients[,1] #new coefficients
    coefs2 <- sort(coefs2)
    if (!is.null(newmodel@optinfo$conv$lme4$code)) {
  	  nonconvmodels = nonconvmodels + 1
    }
    for (j in 1:length(coefs)){
   if (names(coefs[j])=="generation") { #we are not interested in non-significant coefficients
        if (sign(coefs[j])!=sign(coefs2[j])) {
  	      infl_points1 = infl_points1 + 1
        } else if (abs((coefs[j]-coefs2[j])/coefs2[j])>0.2){
  	       infl_points2 = infl_points2 + 1
          }
  	  }  
  	  if (abs(coefs2[j]-coefs[j])>abs(maxdiffs[j])) {
  	    maxdiffs[j] = coefs2[j]-coefs[j]
  	  }
      }
    }
    
  print("If we exclude datapoints one by one...")
  print(paste("...there are ", nonconvmodels, " non-converging models" ))
  print(paste("..there are ", infl_points1, " cases when a significant coefficient changes sign" ))
  print(paste("There are ", infl_points2, " cases when the absolute relative change of a significant coefficient is larger than 0.2"))
  
  #The maximum differences are as follows:
  print(maxdiffs)
  #on the relative scale:
  print(maxdiffs/coefs)
  
}

if (learnability) {
  measure = "fidelity"
  library("effsize")
  epsilon_n <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_i <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_d <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_p.csv",sep=""),header=T,sep=";",dec=".")

  print(t.test(epsilon_n[epsilon_n$generation == 1,]$mvalue,epsilon_n[epsilon_n$generation == 10,]$mvalue,paired=TRUE))
  print(t.test(epsilon_i[epsilon_i$generation == 1,]$mvalue,epsilon_i[epsilon_i$generation == 10,]$mvalue,paired=TRUE))
  print(t.test(epsilon_d[epsilon_d$generation == 1,]$mvalue,epsilon_d[epsilon_d$generation == 10,]$mvalue,paired=TRUE))

  print(cohen.d(epsilon_n[epsilon_n$generation == 1,]$mvalue,epsilon_n[epsilon_n$generation == 10,]$mvalue,paired=TRUE))
  print(cohen.d(epsilon_i[epsilon_i$generation == 1,]$mvalue,epsilon_i[epsilon_i$generation == 10,]$mvalue,paired=TRUE))
  print(cohen.d(epsilon_d[epsilon_d$generation == 1,]$mvalue,epsilon_d[epsilon_d$generation == 10,]$mvalue,paired=TRUE))

}

if (learnability_as_function_of_expressibility){
  measure = "fitness"
  eps_fitness <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,".csv",sep=""),dec=".",sep=";",header = TRUE)
  
  #center the predictor (convenient since we are going to square it)
  eps_fitness$overspec_vgs <- scale(eps_fitness$overspec_vg,scale = FALSE)
  
  #square the predictor
  eps_fitness$overspec_vg2 <- eps_fitness$overspec_vgs ^ 2
  
  #set imperfect learners as reference level
  eps_fitness$learner <- relevel(eps_fitness$learner, ref = "l2")
  
  #model without fixed effects
  null_model <- lmer(next_fidelity ~ (1 + overspec_vg2 + overspec_vgs  | chain), data=eps_fitness)
  
  #we add expressibility as a fixed effect
  overspec_only <- lmer(next_fidelity ~ overspec_vgs + (1 + overspec_vg2 + overspec_vgs  | chain), data=eps_fitness)
  print(anova(null_model,overspec_only))
  #no significant difference
  
  #we add both expressibility and squared expressibility as fixed effects
  overspec2 <- lmer(next_fidelity ~ overspec_vg2 + overspec_vgs + (1 + overspec_vg2 + overspec_vgs | chain), data=eps_fitness)
  print(anova(null_model,overspec2))
  #overspec2 is significantly better
  
  #would it be enough to add only squared expressibility?
  overspec2o <- lmer(next_fidelity ~ overspec_vg2  + (1 + overspec_vg2 + overspec_vgs | chain), data=eps_fitness)
  print(anova(overspec2o,overspec2))
  #no significant difference. Since overspec2o is more parsimonious than overspec2, we consider it the best model so far.
  
  #we add learner type and its interaction with squared expressibility as fixed effects
  overspec2ol <- lmer(next_fidelity ~ overspec_vg2 * learner + (1 + overspec_vg2 + overspec_vgs | chain), data=eps_fitness)
  print(anova(overspec2o,overspec2ol))
  #overspec2ol is the best model
  
  library("languageR")
  print(collin.fnc(eps_fitness,c(6,7)))$cnumber #is there any collinearity between the fixed effects?
  #no collinearity (see Baayen 2008)
 
  fitness_bestmodel <- overspec2ol

  print(summary(fitness_bestmodel))
  print(confint(fitness_bestmodel,"beta_")) #profile confidence intervals
  
  #random effect coefficients
  print(coef(fitness_bestmodel))
    
  #test assumptions
  plot(fitted(fitness_bestmodel),residuals(fitness_bestmodel)) #Some heteroskedasticity
  dev.new()
  hist(residuals(fitness_bestmodel)) #testing for normality
  dev.new()
  qqnorm(residuals(fitness_bestmodel)) #testing for normality
  dev.new()
  print(shapiro.test(residuals(fitness_bestmodel))) #the residuals are not normally distributed. Gelman and Hill (2007: 46) describe the assumption of the normality of residuals as "least important", and for the purpose of estimating the regression line even "barely important".

  boxplot(residuals(fitness_bestmodel) ~ eps_fitness$learner) #Mild heteroskedasticity: less variance for normal learners 
  dev.new()
  boxplot(residuals(fitness_bestmodel) ~ eps_fitness$overspec_vg2) #Very mild heteroskedasticity across different values of the continuous predictor   
  
  #Do we have influential data points? 
  coefs = summary(fitness_bestmodel)$coefficients[,1] #original coefficients
  coefs <- sort(coefs)
  maxdiffs = numeric(length(coefs))
  names(maxdiffs) = names(coefs)
  nonconvmodels = 0
  infl_points1 = 0
  infl_points2 = 0
  
  #exclude every single data point and test if there are significant changes in the coefficients
  for (i in 1:nrow(eps_fitness)) {
    eps_fitness_excluded = eps_fitness[-i,] #exclude a row (=given chain, given generation)
    newmodel = lmer(next_fidelity ~ overspec_vg2 * learner + (1 + overspec_vg2 + overspec_vgs | chain), data=eps_fitness_excluded)
    coefs2 = summary(newmodel)$coefficients[,1] #new coefficients
    coefs2 <- sort(coefs2)
    if (!is.null(newmodel@optinfo$conv$lme4$code)) {
  	  nonconvmodels = nonconvmodels + 1
    }
    for (j in 1:length(coefs)){
   if (names(coefs[j])=="overspec_vg2" | names(coefs[j])=="learnerl1")   { #we are not interested in non-significant coefficients
        if (sign(coefs[j])!=sign(coefs2[j])) {
          infl_points1 = infl_points1 + 1
        } else if (abs((coefs[j]-coefs2[j])/coefs2[j])>0.2){
            infl_points2 = infl_points2 + 1
          }
  	  }  
  	  if (abs(coefs2[j]-coefs[j])>abs(maxdiffs[j])) {
  	    maxdiffs[j] = coefs2[j]-coefs[j]
  	  }
      }
    }
    
  print("If we exclude datapoints one by one...")
  print(paste("...there are ", nonconvmodels, " non-converging models" ))
  print(paste("..there are ", infl_points1, " cases when a significant coefficient changes sign" ))
  print(paste("There are ", infl_points2, " cases when the absolute relative change of a significant coefficient is larger than 0.2"))
  
  #The maximum differences are as follows:
  print(maxdiffs)
  #on the relative scale:
  print(maxdiffs/coefs)
  
}