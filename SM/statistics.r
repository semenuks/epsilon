analyzettr = FALSE # set to TRUE to reproduce the analysis for section 4.2
analyzeexpr = FALSE # set to TRUE to reproduce the analysis for section 4.3
analyzecompr = FALSE # set to TRUE to reproduce the analysis for section 4.4
analyzedttr = TRUE # set to TRUE to reproduce the LMM and MOB analysis for section 4.5
analyzechangerate = FALSE # set to TRUE to reproduce the t-test for section 4.5

#set working directory
#setwd(".\\Analyzed")

#load packages
library ("lme4")
library("party")

#function for calculating R-squared for random trees
accuracy <- function(dataset,forest,oob=TRUE){
  predicted <- predict(forest,OOB=oob)
  y_mean <- mean(dataset$mvalue)
  sum_squared_residuals <- sum((dataset$mvalue-predicted)^2)
  sum_squares_total <- sum((dataset$mvalue-y_mean)^2)
  return(1-sum_squared_residuals/sum_squares_total)
}

ver = 336 #legacy variable. Has to be the same as in data_analyzer.rb
set.seed(3202) #random trees will be slightly different with a different random seed

if (analyzettr==TRUE){
  #TTR ANALYSIS
  measure = "ttr" 
  
  #read data
  epsilon_n <- read.csv(paste("v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste("v",ver,"_",measure,"_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste("v",ver,"_",measure,"_p.csv",sep=""),header=T,sep=";",dec=".")
  
  #mark conditions
  epsilon_n$condition = "n" #normal 
  epsilon_t$condition = "t" #temp. interrupted
  epsilon_p$condition = "_p" #perm. interrupted

  #mark learner types
  epsilon_n$learnertype="l1" #l1 = normal learner 
  epsilon_t$learnertype="l1"
  epsilon_p$learnertype="l1"
  epsilon_t[epsilon_t$generation==2 | epsilon_t$generation==3 | epsilon_t$generation==4,]$learnertype="l2"
  epsilon_p[epsilon_p$generation>1,]$learnertype="l2" #l2 = imperfect learner


  epsilon <- rbind(epsilon_n,epsilon_t,epsilon_p)

 
  #transform variables
  epsilon$condition <- as.factor(epsilon$condition)
  epsilon$chain2 <- as.factor(epsilon$chain)
  
  #The maximal model
  regrmax <- lmer(mvalue ~ generation*condition + (1+generation|chain2),data = epsilon, REML = TRUE)
  
  bestmodel <- regrmax
  print(summary(bestmodel))
 
  #random effect coefficients
  print(coef(bestmodel))
   
  #test significance via confidence intervals
  print(confint(bestmodel,"beta_"))
  #generation and generation:conditionn are signicant, other aren't (CI contains 0)
  
  #test assumptions
  plot(fitted(bestmodel),residuals(bestmodel)) #heteroskedasticity and linearity
  dev.new()
  hist(residuals(bestmodel)) #normality
  dev.new()
  qqnorm(residuals(bestmodel)) #normality
  dev.new()
  print(shapiro.test(residuals(bestmodel)))
  #the residuals are not normally distributed
  
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
    newmodel = lmer(mvalue ~ condition * generation + (1+generation|chain2), data = epsilon_excluded, REML = TRUE)
    coefs2 = summary(newmodel)$coefficients[,1] #new coefficients
	coefs2 <- sort(coefs2)
    if (!is.null(newmodel@optinfo$conv$lme4$code)) {
  	  #print(paste("The model does not converge without the row ",i, "chain ", epsilon$chain[i], "generation ", epsilon$generation[i]))
  	  nonconvmodels = nonconvmodels + 1
    }
    for (j in 1:length(coefs)){
	  if (names(coefs[j])=="generation" | names(coefs[j]) =="generation:conditionn") { #we are not interested in non-significant coefficients
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
    
  print("If we exclude datapoints one by one...")
  print(paste("...there are ", nonconvmodels, " non-converging models" ))
  print(paste("..there are ", infl_points1, " cases when a significant coefficient changes sign" ))
  print(paste("There are ", infl_points2, " cases when the absolute relative change of a significant coefficient is larger than 0.2"))
  
  #The maximum differences are as follows:
  print(maxdiffs)
  #on the relative scale:
  print(maxdiffs/coefs)
  
  #Random tree
  ttr.tree <- ctree(mvalue~condition+generation,data=epsilon)
  plot(ttr.tree)
  #prediction accuracy
  print(accuracy(epsilon,ttr.tree))
  
  #model-based recursive partitioning #not reported in the article
  ttrmob<-mob(mvalue~generation|condition,data=epsilon)
  dev.new()
  plot(ttrmob)
  print(summary(ttrmob))
  print(accuracy(epsilon,ttrmob))

#what happens if we concentrate on generations 5-10? Fig. 4 suggests that post-contact slopes are similar for conditions N and T, but different for P  
  
  #delete generations 0-4
  epsilon1 <- epsilon[epsilon$generation>4,]
  regrmax1 <- lmer(mvalue ~ generation*condition + (1+generation|chain2),data = epsilon1, REML = TRUE)
  print(summary(regrmax1))
  print(confint(regrmax1,"beta_"))
   
  #random effect coefficients
  print(coef(bestmodel1))
   
  #test significance via confidence intervals
  print(confint(bestmodel1,"beta_"))
  #generation and generation:conditionn are signicant, other aren't (CI contains 0)
  
  #test assumptions
  plot(fitted(bestmodel1),residuals(bestmodel1)) #heteroskedasticity and linearity
  dev.new()
  hist(residuals(bestmodel1)) #normality
  dev.new()
  qqnorm(residuals(bestmodel1)) #normality
  dev.new()
  print(shapiro.test(residuals(bestmodel1)))
  #the residuals are not normally distributed
  
  #Do we have influential data points? 
  coefs = summary(bestmodel1)$coefficients[,1] #original coefficients
  coefs <- sort(coefs)
  maxdiffs = numeric(length(coefs))
  names(maxdiffs) = names(coefs)
  nonconvmodels = 0
  infl_points1 = 0
  infl_points2 = 0
  
  #exclude every single data point and test if there are significant changes in the coefficients
  for (i in 1:nrow(epsilon1)) {
    epsilon_excluded = epsilon1[-i,] #exclude a row (=given chain, given generation)
    newmodel = lmer(mvalue ~ condition * generation + (1+generation|chain2), data = epsilon_excluded, REML = TRUE)
    coefs2 = summary(newmodel)$coefficients[,1] #new coefficients
	coefs2 <- sort(coefs2)
    if (!is.null(newmodel@optinfo$conv$lme4$code)) {
  	  #print(paste("The model does not converge without the row ",i, "chain ", epsilon$chain[i], "generation ", epsilon$generation[i]))
  	  nonconvmodels = nonconvmodels + 1
    }
    for (j in 1:length(coefs)){
	  if (names(coefs[j])=="generation") { #we are not interested in non-significant coefficients
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
    
  print("If we exclude datapoints one by one...")
  print(paste("...there are ", nonconvmodels, " non-converging models" ))
  print(paste("..there are ", infl_points1, " cases when a significant coefficient changes sign" ))
  print(paste("There are ", infl_points2, " cases when the absolute relative change of a significant coefficient is larger than 0.2"))
  
  #The maximum differences are as follows:
  print(maxdiffs)
  #on the relative scale:
  print(maxdiffs/coefs)
  
}

#EXPRESSIBILITY ANALYSIS
if (analyzeexpr==TRUE) {
  measure = "expr" 
  
  #read data and mark relevant variables
  epsilon_n <- read.csv(paste("v",ver,"_",measure,"_noun_number_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste("v",ver,"_",measure,"_noun_number_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste("v",ver,"_",measure,"_noun_number_p.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_n$condition="n"
  epsilon_t$condition="t"
  epsilon_p$condition="_p"
  epsilon_nouns <- rbind(epsilon_n,epsilon_t,epsilon_p)
  epsilon_nouns$mcategory <- "number"
  epsilon_nouns$lex_or_gram <- "gram"
  epsilon_nouns$pos <- "noun"
  
  epsilon_n <- read.csv(paste("v",ver,"_",measure,"_verb_gender_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste("v",ver,"_",measure,"_verb_gender_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste("v",ver,"_",measure,"_verb_gender_p.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_n$condition="n"
  epsilon_t$condition="t"
  epsilon_p$condition="_p"
  epsilon_verbs <- rbind(epsilon_n,epsilon_t,epsilon_p)
  epsilon_verbs$mcategory <- "_agreement"
  epsilon_verbs$lex_or_gram <- "gram"
  epsilon_verbs$pos <- "verb"
  
  epsilon_n <- read.csv(paste("v",ver,"_",measure,"_verb_lex_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste("v",ver,"_",measure,"_verb_lex_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste("v",ver,"_",measure,"_verb_lex_p.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_n$condition="n"
  epsilon_t$condition="t"
  epsilon_p$condition="_p"
  epsilon_lverbs <- rbind(epsilon_n,epsilon_t,epsilon_p)
  epsilon_lverbs$mcategory <- "event"
  epsilon_lverbs$lex_or_gram <- "_lex"
  epsilon_lverbs$pos <- "verb"
  
  epsilon_n <- read.csv(paste("v",ver,"_",measure,"_noun_lex_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste("v",ver,"_",measure,"_noun_lex_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste("v",ver,"_",measure,"_noun_lex_p.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_n$condition="n"
  epsilon_t$condition="t"
  epsilon_p$condition="_p"
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
  
  expr_regrmax <- lmer(mvalue ~ generation*condition*mcategory + (1+generation*mcategory|chain2),data=epsilon2,REML=TRUE)
  #does not converge. We start the simplification
  
  expr_regrmax_uncorr <- lmer(mvalue ~ generation*condition*mcategory + (1|chain2) + (0+generation|chain2) + (0+mcategory|chain2)+(0+generation:mcategory|chain2),data=epsilon2,REML=TRUE)
  #does not converge
  
  #no slope for interaction
  expr_nors_int <- lmer(mvalue ~ generation*condition*mcategory + (1|chain2) + (0+generation|chain2) + (0+mcategory|chain2),data=epsilon2,REML=TRUE)
  
  #no slope for generation
  expr_nors_gen <- lmer(mvalue ~ generation*condition*mcategory + (1|chain2) + (0+generation:mcategory|chain2) + (0+mcategory|chain2),data=epsilon2,REML=TRUE)
    
  #no slope for mcategory
  expr_nors_cat <- lmer(mvalue ~ generation*condition*mcategory + (1|chain2) + (0+generation|chain2) + (0+generation:mcategory|chain2),data=epsilon2,REML=TRUE)
  #does not converge
  
  #refit the two candidate models with REML=F for comparing random-effect structure (Winter & Wieling 2016)
  expr_nors_int_f <- lmer(mvalue ~ generation*condition*mcategory + (1|chain2) + (0+generation|chain2) + (0+mcategory|chain2),data=epsilon2,REML=FALSE)
  
  expr_nors_gen_f <- lmer(mvalue ~ generation*condition*mcategory + (1|chain2) + (0+generation:mcategory|chain2) + (0+mcategory|chain2),data=epsilon2,REML=FALSE)
  
  print(anova(expr_nors_int_f,expr_nors_gen_f))
  #expr_nors_gen is better
  
  #restoring the correlation coefficients: both are back
  expr_nors_gen_corr <- lmer(mvalue ~ generation*condition*mcategory + (1+generation:mcategory|chain2) + (1+mcategory|chain2),data=epsilon2,REML=FALSE)
  #does not converge
  
  #corr.coef only for interaction
  expr_nors_gen_corr2 <- lmer(mvalue ~ generation*condition*mcategory + (1|chain2) + (1+generation:mcategory|chain2) + (0+mcategory|chain2),data=epsilon2,REML=FALSE)
  #does not converge
  
  #corr.coef only for category
  expr_nors_gen_corr3 <- lmer(mvalue ~ generation*condition*mcategory + (1|chain2) + (0+generation:mcategory|chain2) + (1+mcategory|chain2),data=epsilon2,REML=FALSE)
  #does not converge
  
  #print(confint(expr_nors_gen,"beta_")) #profiling over both the residual variance and fixed effects is not numerically consistent with profiling over the fixed effects only
  #we have to simplify further
  
  #no random slope for generation, no random slope for interaction
  expr_nors_gen_int <- lmer(mvalue ~ generation*condition*mcategory + (1|chain2) + (0+mcategory|chain2),data=epsilon2,REML=TRUE)
  #print(confint(expr_nors_gen_int,"beta_"))
  #same problem
  
  expr_nors_gen_cat <- lmer(mvalue ~ generation*condition*mcategory + (1|chain2) + (0+generation:mcategory|chain2),data=epsilon2,REML=TRUE)
  #print(confint(expr_nors_gen_cat,"beta_"))
  #same problem
  
  expr_nors <- lmer(mvalue ~ generation*condition*mcategory + (1|chain2),data=epsilon2,REML=TRUE)
  print(confint(expr_nors,"beta_"))
  #OK.
  
  expr_bestmodel <- expr_nors
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
  
  #random-tree analysis
  #single tree for expr without chain
  expr.tree <- ctree(mvalue~condition+mcategory+generation,data=epsilon2)
  plot(expr.tree)
  #prediction accuracy
  print(accuracy(epsilon2,expr.tree))
  
  #minbucket=200 (do not create bins that contain less than 200 observations) shows the pattern more clearly (though the at the cost of lower accuracy)
  expr.tree200 <- ctree(mvalue~condition+mcategory+generation,data=epsilon2,controls=ctree_control(minbucket=200))
  dev.new()
  plot(expr.tree200)
  #prediction accuracy is somewhat lower
  print(accuracy(epsilon2,expr.tree200))
  
  #model-based recursive partitioning
  exprmob<-mob(mvalue~generation|condition+mcategory,data=epsilon2)
  dev.new()
  plot(exprmob)
  print(coef(exprmob))
  print(accuracy(epsilon2,exprmob))
  
  #is agreement different just because it's both verbal and grammatical?
  expr1 <- lmer(mvalue ~ generation*condition*lex_or_gram*pos + (1|chain2),data=epsilon2,REML=TRUE)
  print(summary(expr1))
  print(confint(expr1,"beta_"))
  
  #addressing the same question by means of random trees
  #single tree for expr only with lex and gram
  iexpr.tree <- ctree(mvalue~lex_or_gram+pos,data=epsilon2)
  dev.new()
  plot(iexpr.tree)
  #prediction accuracy
  print(accuracy(epsilon2,iexpr.tree))
  
  #single tree for expr with all predictors
  iexpr.tree2 <- ctree(mvalue~lex_or_gram+pos+condition+generation,data=epsilon2)
  dev.new()
  plot(iexpr.tree2)
  #prediction accuracy
  print(accuracy(epsilon2,iexpr.tree2))
    
  #minbucket=200 (do not create bins that contain less than 200 observations) shows the pattern more clearly (though the at the cost of lower accuracy)
  iexpr.tree200 <- ctree(mvalue~lex_or_gram+pos+condition+generation,data=epsilon2,controls=ctree_control(minbucket=200))
  dev.new()
  plot(iexpr.tree200)
  #prediction accuracy
  print(accuracy(epsilon2,iexpr.tree200))
  
  #model-based recursive partitioning
  iexprmob <- mob(mvalue~generation|condition+lex_or_gram+pos,data=epsilon2)
  dev.new()
  plot(iexprmob)
  print(coef(iexprmob)) #the model coefficients
  print(accuracy(epsilon2,iexprmob))

}


if (analyzecompr==TRUE) {
  measure = "comprehension_rate" 
  epsilon_n <- read.csv(paste("v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste("v",ver,"_",measure,"_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste("v",ver,"_",measure,"_p.csv",sep=""),header=T,sep=";",dec=".")
  
  #mark conditions
  epsilon_n$condition = "n" #normal 
  epsilon_t$condition = "t" #temp. interrupted
  epsilon_p$condition = "_p" #perm. interrupted
  epsilon4 <- rbind(epsilon_n,epsilon_t,epsilon_p)
  #delete generation 0
  epsilon4 <- epsilon4[epsilon4$generation!=0,]
  
  #transform variables
  epsilon4$condition <- as.factor(epsilon4$condition)
  epsilon4$chain2 <- as.factor(epsilon4$chain)
  
  #maximal model
  compr_max <- lmer(mvalue ~ generation*condition + (1+generation|chain2),data = epsilon4, REML = TRUE)
  print(confint(compr_max,"beta_")) 
  
  compr_bestmodel <- compr_max
  print(summary(compr_bestmodel))
  
  #coefficients for random effects
  print(coef(compr_bestmodel))
  
  #test assumptions
  plot(fitted(compr_bestmodel),residuals(compr_bestmodel)) #heteroskedasticity and linearity
  dev.new()
  hist(residuals(compr_bestmodel)) #normality
  dev.new()
  qqnorm(residuals(compr_bestmodel)) #normality
  dev.new()
  print(shapiro.test(residuals(compr_bestmodel)))
  #the residuals are not normally distributed
  
  #Do we have influential data points? 
  coefs = summary(compr_bestmodel)$coefficients[,1] #original coefficients
  coefs <- sort(coefs)
  maxdiffs = numeric(length(coefs))
  names(maxdiffs) = names(coefs)
  nonconvmodels = 0
  infl_points1 = 0
  infl_points2 = 0
  
  #exclude every single data point and test if there are significant changes in the coefficients
  for (i in 1:nrow(epsilon4)) {
    epsilon_excluded4 = epsilon4[-i,] #exclude a row (=given chain, given generation)
    newmodel = lmer(mvalue ~ condition * generation + (1|chain2), data = epsilon_excluded4, REML = TRUE)
    coefs2 = summary(newmodel)$coefficients[,1] #new coefficients
	coefs2 <- sort(coefs2)
    if (!is.null(newmodel@optinfo$conv$lme4$code)) {
  	  #print(paste("The model does not converge without the row ",i, "chain ", epsilon$chain[i], "generation ", epsilon$generation[i]))
  	  nonconvmodels = nonconvmodels + 1
    }
    for (j in 1:length(coefs)){
      if (names(coefs[j])=="conditiont" | names(coefs[j]) =="generation:conditionp") {
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
  
  print("If we exclude datapoints one by one...")
  print(paste("...there are ", nonconvmodels, " non-converging models" ))
  print(paste("..there are ", infl_points1, " influential datapoints (according to the 'coefficient changes sign' criterion); only for significant coefficients" ))
  print(paste("There are ", infl_points2, " influential datapoints (according to the 'absolute relative change of the coefficient is larger than 0.2' criterion); only for significant coefficients"))
  
  #The maximum differences are as follows:
  print(maxdiffs)
  #on the relative scale:
  print(maxdiffs/coefs)
  
  #single tree for compr 
  compr.tree <- ctree(mvalue~condition+generation,data=epsilon4)
  plot(compr.tree)
  #prediction accuracy
  print(accuracy(epsilon4,compr.tree))
  
  #model-based recursive partitioning
  comprmob<-mob(mvalue~generation|condition,data=epsilon4)
  dev.new()
  plot(comprmob)
  print(coef(comprmob))
  print(accuracy(epsilon4,comprmob))
}

if (analyzedttr==TRUE){
  measure = "dttr" 
  epsilon_n <- read.csv(paste("v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste("v",ver,"_",measure,"_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste("v",ver,"_",measure,"_p.csv",sep=""),header=T,sep=";",dec=".")
  
  #mark conditions
  epsilon_n$condition = "n" #normal 
  epsilon_t$condition = "t" #temp. interrupted
  epsilon_p$condition = "_p" #perm. interrupted
  
  #mark learner types
  epsilon_n$learnertype="l1" #l1 = normal learner
  epsilon_t$learnertype="l1"
  epsilon_p$learnertype="l1"
  epsilon_t[epsilon_t$generation==2 | epsilon_t$generation==3 | epsilon_t$generation==4,]$learnertype="l2"
  epsilon_p[epsilon_p$generation>1,]$learnertype="l2" #l2 = imperfect learner
  
  epsilon5 <- rbind(epsilon_n,epsilon_t,epsilon_p)
  
  #transform variables
  epsilon5$condition <- as.factor(epsilon5$condition)
  epsilon5$chain2 <- as.factor(epsilon5$chain)
  epsilon5$learnertype <- as.factor(epsilon5$learnertype)
  
  dttr_max <- lmer(mvalue ~ learnertype*prev_value + (1+prev_value|chain2),data=epsilon5,REML=TRUE)
  #confint(dttr_max,"beta_")
  #error while calculating CIs. We have to simplify
  
  #no correlation coefficient
  dttr_max_uncorr <- lmer(mvalue ~ learnertype*prev_value + (1|chain2) + (0+prev_value|chain2),data=epsilon5,REML=TRUE)
  #confint(dttr_max_uncorr,"beta_")
  #error
  
  #no random slope
  dttr_max_nors <- lmer(mvalue ~ learnertype*prev_value + (1|chain2),data=epsilon5,REML=TRUE)
  #confint(dttr_max_nors,"beta_")
  #warnings! We have to simplify the fixed effect structure

  #no interaction
  dttr_noint_nors <- lmer(mvalue ~ learnertype+prev_value + (1|chain2),data=epsilon5,REML=TRUE)
  confint(dttr_noint_nors,"beta_")
  #OK
  
  #should we have removed prev_value instead of the interaction? 
  dttr_nopv_nors <- lmer(mvalue ~ learnertype+learnertype:prev_value + (1|chain2),data=epsilon5,REML=TRUE)
  #confint(dttr_nopv_nors,"beta_")
  #error
  
  #should we have removed learnertype instead of the interaction? 
  dttr_nolt_nors <- lmer(mvalue ~ prev_value+learnertype:prev_value + (1|chain2),data=epsilon5,REML=TRUE)
  confint(dttr_nolt_nors,"beta_")
  #OK.
  
  #Which candidate model is better?
  print(anova(dttr_noint_nors,dttr_nolt_nors))
  #The one with the interaction, but without the learner type
  
  #Can we restore random effects now?
  dttr_nolt_rs_pv <- lmer(mvalue ~ learnertype:prev_value+prev_value + (1+prev_value|chain2),data=epsilon5,REML=TRUE)
  #confint(dttr_nolt_rs_pv,"beta_")
  #error
  
  #without the correlation coefficient?
  dttr_nolt_rs_pv_uncorr <- lmer(mvalue ~ learnertype:prev_value+prev_value + (1|chain2) +  (0+prev_value|chain2),data=epsilon5,REML=TRUE)
  #confint(dttr_nolt_rs_pv_uncorr,"beta_")
  #error

  dttr_bestmodel <- dttr_nolt_nors
  print(summary(dttr_bestmodel))
 
  #random effect coefficients
  print(coef(dttr_bestmodel))

  plot(fitted(dttr_bestmodel),residuals(dttr_bestmodel)) #heteroskedasticity and linearity
  #two outliers outside the blob, but that should be a major problem
  dev.new()
  hist(residuals(dttr_bestmodel)) #normality
  dev.new()
  qqnorm(residuals(dttr_bestmodel)) #normality
  dev.new()
  print(shapiro.test(residuals(dttr_bestmodel)))
  #the residuals are not normally distributed
  
  #Do we have influential data points? 
  coefs = summary(dttr_bestmodel)$coefficients[,1] #original coefficients
  coefs <- sort(coefs)
  maxdiffs = numeric(length(coefs))
  names(maxdiffs) = names(coefs)
  nonconvmodels = 0
  infl_points1 = 0
  infl_points2 = 0
  
  #exclude every single data point and test if there are significant changes in the coefficients
  for (i in 1:nrow(epsilon5)) {
    epsilon_excluded = epsilon5[-i,] #exclude a row (=given chain, given generation)
    newmodel = lmer(mvalue ~ prev_value + learnertype:prev_value  + (1+generation|chain2), data = epsilon_excluded, REML = TRUE)
    coefs2 = summary(newmodel)$coefficients[,1] #new coefficients
	coefs2 <- sort(coefs2)
    if (!is.null(newmodel@optinfo$conv$lme4$code)) {
  	  #print(paste("The model does not converge without the row ",i, "chain ", epsilon$chain[i], "generation ", epsilon$generation[i]))
  	  nonconvmodels = nonconvmodels + 1
    }
    for (j in 1:length(coefs)){
	  if (names(coefs[j])=="prev_value:learnertype" | names(coefs[j]) =="prev_value") { #we are not interested in non-significant coefficients
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
    
  print("If we exclude datapoints one by one...")
  print(paste("...there are ", nonconvmodels, " non-converging models" ))
  print(paste("..there are ", infl_points1, " cases when a significant coefficient changes sign" ))
  print(paste("There are ", infl_points2, " cases when the absolute relative change of a significant coefficient is larger than 0.2"))
  
  #The maximum differences are as follows:
  print(maxdiffs)
  #on the relative scale:
  print(maxdiffs/coefs)
  
  #single tree for dTTR 
  dttr.tree <- ctree(mvalue~learnertype+prev_value,data=epsilon5)
  plot(dttr.tree)
  #prediction accuracy
  print(accuracy(epsilon5,dttr.tree))
  
  #model-based recursive partitioning
  stypemob<-mob(mvalue~prev_value|learnertype,data=epsilon5)
  dev.new()
  plot(stypemob)
  print(coef(stypemob))
  print(accuracy(epsilon5,stypemob))
}

if (analyzechangerate==TRUE){
  measure = "change_rate" #=transmission error 
  
  #read data
  epsilon_n <- read.csv(paste("v",ver,"_",measure,"_n.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_t <- read.csv(paste("v",ver,"_",measure,"_t.csv",sep=""),header=T,sep=";",dec=".")
  epsilon_p <- read.csv(paste("v",ver,"_",measure,"_p.csv",sep=""),header=T,sep=";",dec=".")
  
  #mark conditions
  epsilon_n$condition = "n" #normal 
  epsilon_t$condition = "t" #temp. interrupted
  epsilon_p$condition = "_p" #perm. interrupted
 
  #mark learner types
  epsilon_n$learnertype="l1"#l1 = normal learner
  epsilon_t$learnertype="l1"
  epsilon_p$learnertype="l1"
  epsilon_t[epsilon_t$generation==2 | epsilon_t$generation==3 | epsilon_t$generation==4,]$learnertype="l2"
  epsilon_p[epsilon_p$generation>1,]$learnertype="l2" #l2 = imperfect learner
   
  epsilon6 <- rbind(epsilon_n,epsilon_t,epsilon_p)
  
  #is there a significant difference at generation 2?
  print(t.test(epsilon6[epsilon6$generation==2 & epsilon6$learnertype == "l1",]$mvalue,epsilon6[epsilon6$generation==2 & epsilon6$learnertype == "l2",]$mvalue))
 
  #at generation 1? (NO)
  #t.test(epsilon6[epsilon6$generation==1 & epsilon6$condition == "n",]$mvalue,epsilon6[epsilon6$generation==1 & (epsilon6$condition == "t" | epsilon6$condition == "_p"),]$mvalue)
 
  #t-tests for TTR at generation 2, 4 and 10 (not reported in the article. Keep in mind the multiple comparisons issue)
  #t.test(epsilon[epsilon$generation==2 & epsilon$learnertype == "l1",]$mvalue,epsilon[epsilon$generation==2 & epsilon$learnertype == "l2",]$mvalue)
  #t.test(epsilon[epsilon$generation==4 & epsilon$learnertype == "l1",]$mvalue,epsilon[epsilon$generation==4 & epsilon$learnertype == "l2",]$mvalue)
  #t.test(epsilon[epsilon$generation==10 & epsilon$learnertype == "l1",]$mvalue,epsilon[epsilon$generation==10 & epsilon$learnertype == "l2",]$mvalue)
   
}