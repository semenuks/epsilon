#This script performs the statistical tests mentioned in the articles. Set the value for the measure you are interested in to TRUE below. Make sure the folder Measures contains the necessary data (=output of the script data_analyzer.rb)

#which measures should be analyzed
ttr = TRUE
ttr_nouns = FALSE
ttr_verbs = FALSE
ttr_v_stem = FALSE
ttr_v_affix = FALSE
fidelity = FALSE
expressibility = FALSE

#load packages
library ("lme4")
library("lmerTest")
library("effsize")
ver = 364 #legacy variable. Has to be the same as in data_analyzer.rb


read_dataset <- function(measure, ver, transformation){
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
  epsilon$learnertype2 <- as.factor(epsilon$learnertype)
  epsilon$condition2 <- as.factor(epsilon$condition)
#contrasts(epsilon$condition2) = contr.poly(3)
  epsilon$chain2 <- as.factor(epsilon$chain)
  
  if (transformation == "log") {
    epsilon$mvalue2 <- log(epsilon$mvalue) #log transformation to prevent the model from predicting negative TTR for generations after 10. We do not have to correct for non-zero values, since TTR > 0  
  } else if (transformation == "logplus") {
    epsilon$mvalue2 <- log(epsilon$mvalue + 1) #expressibility can equal 0, we have to add 1 
  }
  return(epsilon)
}

fit_lmm <- function(epsilon){
  #The maximal model
  bestmodel <-  lmer(mvalue2 ~ generation*condition2 + (1+generation|chain2), data = epsilon, REML = TRUE)
  
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

if (ttr) {
  #TTR ANALYSIS (Section 3.3)
  measure = "ttr" 
  epsilon <- read_dataset(measure, ver, "log")
  print(measure)
  #the t-test described at the end of 3.3
  print(t.test(epsilon[epsilon$generation==2 & epsilon$learnertype == "l1",]$mvalue, epsilon[epsilon$generation==2 & epsilon$learnertype == "l2",]$mvalue))
  print(cohen.d(epsilon[epsilon$generation==2 & epsilon$learnertype == "l1",]$mvalue,epsilon[epsilon$generation==2 & epsilon$learnertype == "l2",]$mvalue))

  #Fitting LMM
  fit_lmm(epsilon)
}

if (ttr_nouns) {
  #TTR OF NOUNS ONLY (Section 3.4)
  measure = "ttr_nouns" 
  epsilon <- read_dataset(measure, ver, "log")
  print(measure)
  # T-test comparing generation 0 and 10
  print(t.test(epsilon[epsilon$generation==0 & epsilon$condition2 == "n",]$mvalue, epsilon[epsilon$generation==10 & epsilon$condition2 == "n",]$mvalue))
  print(cohen.d(epsilon[epsilon$generation==0 & epsilon$condition2 == "n",]$mvalue, epsilon[epsilon$generation==10 & epsilon$condition2 == "n",]$mvalue))
  print(t.test(epsilon[epsilon$generation==0 & epsilon$condition2 == "_t",]$mvalue, epsilon[epsilon$generation==10 & epsilon$condition2 == "_t",]$mvalue))
  print(cohen.d(epsilon[epsilon$generation==0 & epsilon$condition2 == "_t",]$mvalue, epsilon[epsilon$generation==10 & epsilon$condition2 == "_t",]$mvalue))
  print(t.test(epsilon[epsilon$generation==0 & epsilon$condition2 == "p",]$mvalue, epsilon[epsilon$generation==10 & epsilon$condition2 == "p",]$mvalue))
  print(cohen.d(epsilon[epsilon$generation==0 & epsilon$condition2 == "p",]$mvalue, epsilon[epsilon$generation==10 & epsilon$condition2 == "p",]$mvalue))

}

if (ttr_verbs) {
  #TTR OF VERBS ONLY (Section 3.4)
  measure = "ttr_verbs" 
  epsilon <- read_dataset(measure, ver, "log")
  print(measure)
  #Fitting the LMM mentioned in 3.4 and described in S6
  fit_lmm(epsilon)
}

if (ttr_v_affix) {
  #TTR OF VERB ENDINGS (Section 3.4)
  measure = "ttr_v_affix" 
  epsilon <- read_dataset(measure, ver, "log")
  print(measure)
  fit_lmm(epsilon)
}

if (ttr_v_stem) {
  #TTR OF VERB STEMS (Section 3.4)
  measure = "ttr_v_stem" 
  epsilon <- read_dataset(measure, ver, "log")
  print(measure)
  fit_lmm(epsilon)
}

if (fidelity){
  #Transmission fidelity (Section 3.2)
  measure = "fidelity" #=transmission error 
  epsilon <- read_dataset(measure, ver, "log")
  print(measure)
  
  #is there a significant difference at generation 2?
  print(t.test(epsilon[epsilon$generation==2 & epsilon$learnertype == "l1",]$mvalue,epsilon[epsilon$generation==2 & epsilon$learnertype == "l2",]$mvalue))
 
  #Suppose we run the test comparing N chains with T and P chains at generation 1? (NO DIFFERENCE, AS EXPECTED)
  print(t.test(epsilon[epsilon$generation==1 & epsilon$condition == "n",]$mvalue,epsilon[epsilon$generation==1 & (epsilon$condition == "_t" | epsilon$condition == "p"),]$mvalue))
}

if (expressibility) {
  #Analysis of the expressibility, mentioned in Section 4 and described in S7
  measure = "expr" 
  print(measure)

  #read data and mark relevant variables
  epsilon_n <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_noun_number_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_noun_number_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_noun_number_p.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_n$condition="n"
  epsilon_t$condition="_t"
  epsilon_p$condition="p"
  epsilon_nouns <- rbind(epsilon_n,epsilon_t,epsilon_p)
  epsilon_nouns$mcategory <- "number"
  epsilon_nouns$lex_or_gram <- "gram"
  epsilon_nouns$pos <- "noun"
  
  epsilon_n <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_verb_gender_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_verb_gender_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_verb_gender_p.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_n$condition="n"
  epsilon_t$condition="_t"
  epsilon_p$condition="p"
  epsilon_verbs <- rbind(epsilon_n,epsilon_t,epsilon_p)
  epsilon_verbs$mcategory <- "_agreement"
  epsilon_verbs$lex_or_gram <- "gram"
  epsilon_verbs$pos <- "verb"
  
  epsilon_n <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_verb_lex_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_verb_lex_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_verb_lex_p.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_n$condition="n"
  epsilon_t$condition="_t"
  epsilon_p$condition="p"
  epsilon_lverbs <- rbind(epsilon_n,epsilon_t,epsilon_p)
  epsilon_lverbs$mcategory <- "event"
  epsilon_lverbs$lex_or_gram <- "_lex"
  epsilon_lverbs$pos <- "verb"
  
  epsilon_n <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_noun_lex_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_noun_lex_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste(".\\Measures\\","v",ver,"_",measure,"_noun_lex_p.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_n$condition="n"
  epsilon_t$condition="_t"
  epsilon_p$condition="p"
  epsilon_lnouns <- rbind(epsilon_n,epsilon_t,epsilon_p)
  epsilon_lnouns$mcategory <- "agent"
  epsilon_lnouns$lex_or_gram <- "_lex"
  epsilon_lnouns$pos <- "noun"
  
  epsilon2 <- rbind(epsilon_nouns,epsilon_verbs,epsilon_lverbs,epsilon_lnouns)
  epsilon2$condition <- as.factor(epsilon2$condition)
  epsilon2$mcategory <- as.factor(epsilon2$mcategory)
  epsilon2$chain2 <- as.factor(epsilon2$chain)
  epsilon2$lex_or_gram <- as.factor(epsilon2$lex_or_gram)
  epsilon2$pos <- as.factor(epsilon2$pos)
  
  epsilon2$mvalue2 <- log(epsilon2$mvalue+1)  

  #is agreement different just because it's both verbal and grammatical?
  #expr1 <- lmer(mvalue2 ~ generation*condition*lex_or_gram*pos + (1|chain2),data=epsilon2,REML=TRUE) #does not converge
  
  expr_bestmodel <- lmer(mvalue2 ~ generation * lex_or_gram * pos + (1 + generation|chain2),data=epsilon2,REML=TRUE)
  
  print(summary(expr_bestmodel))

  #coefficients for random effects
  print(coef(expr_bestmodel))
  #nothing particularly remarkable
  
  #test assumptions
  plot(fitted(expr_bestmodel),residuals(expr_bestmodel)) #heteroskedasticity and linearity
  dev.new()
  hist(residuals(expr_bestmodel)) #normality
  dev.new()
  qqnorm(residuals(expr_bestmodel)) #normality
  dev.new()
  print(shapiro.test(residuals(expr_bestmodel)))
  #the residuals are not normally distributed
  
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
    epsilon_excluded2 = epsilon2[-i,] #exclude a row (=given chain, given generation)
    newmodel = lmer(mvalue ~ generation*condition*mcategory + (1|chain2),data=epsilon_excluded2,REML=TRUE)
    coefs2 = summary(newmodel)$coefficients[,1] #new coefficients
	coefs2 <- sort(coefs2)
	#print(coefs2)
    if (!is.null(newmodel@optinfo$conv$lme4$code)) {
  	  #print(paste("The model does not converge without the row ",i, "chain ", epsilon$chain[i], "generation ", epsilon$generation[i]))
  	  nonconvmodels = nonconvmodels + 1
    }
    for (j in 1:length(coefs)){
      if (names(coefs[j])=="generation" | names(coefs[j]) =="mcategoryagent" | names(coefs[j]) =="mcategorynumber" | names(coefs[j]) =="mcategoryevent" | names(coefs[j]) =="generation:conditionn" | names(coefs[j]) =="generation:mcategoryagent" | names(coefs[j]) =="generation:mcategoryevent" | names(coefs[j]) =="generation:mcategorynumber" | names(coefs[j]) =="generation:conditionn:mcategoryagent" | names(coefs[j]) =="generation:conditionn:mcategorynumber") {
        if (sign(coefs[j])!=sign(coefs2[j])) {
  	      #print(paste("Influential data point in row ",i,"chain ", epsilon$chain[i], "generation ", epsilon$generation[i]," The coefficient for ", names(coefs)[j], "changed sign."))
  	      infl_points1 = infl_points1 + 1
  	    } else if (abs((coefs[j]-coefs2[j])/coefs2[j])>0.2){
  	      #print(paste("Quasi-influential data point in row ",i,"chain ", epsilon$chain[i], "generation ", epsilon$generation[i]," The absolute relative change  for the coefficient for ", names(coefs)[j], "is larger than 0.2"))
  	      infl_points2 = infl_points2 + 1
  	    }
  	  }  
  	  if (abs(coefs2[j]-coefs[j])>abs(maxdiffs[j])) {
  	    maxdiffs[j] = coefs2[j]-coefs[j]
  	  }
    }
  }
   
  print ("we finished datapoint analysis") 
  print("If we exclude datapoints one by one...")
  print(paste("...there are ", nonconvmodels, " non-converging models" ))
  print(paste("..there are ", infl_points1, " cases when a significant coefficient changes sign" ))
  print(paste("There are ", infl_points2, " cases when the absolute relative change of a significant coefficient is larger than 0.2"))
  
  #The maximum differences are as follows:
  print(maxdiffs)
  #on the relative scale:
  print(maxdiffs/coefs)
}