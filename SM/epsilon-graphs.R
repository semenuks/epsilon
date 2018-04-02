######
## >> loading up the packages; se function
library(ggplot2)
library(plyr)

se = function(x, na.rm = FALSE) {
  n = ifelse(na.rm, sum(!is.na(x)), length(x))
  return(sd(x, na.rm)/sqrt(n))
}


######
##  ##
######


######
## >> Figure 1. A schematic representation of the iterated learning model in the experiment 
fig1.df.points = data.frame(y = rep(c(6,5.5,5),each=10), x = rep(1:10,3), cond = rep(c('L1','L2L1','L2L2'),each=10), label = c(rep("N",10),"N","I","I","I",rep("N",6),"N",rep("I",9)))
fig1.df.arrows = data.frame(y = rep(c(6,5.5,5),each=11), yend = rep(c(6,5.5,5),each=11), x = (rep(1:11,3)-0.65), xend = (rep(1:11,3)-0.35))

ggplot() + geom_point(data = fig1.df.points, aes(x=x, y=y, color = label, shape = label), size = 12) + geom_text(data = fig1.df.points, aes(x=x+0.015, y=y, label = label), color = "white", fontface="bold") + geom_segment(data = fig1.df.arrows, aes(x = x, xend = xend, y = y, yend = yend), arrow = grid::arrow(length = grid::unit(0.3,"cm"))) + scale_y_continuous(limits = c(4.90,6.25)) + scale_x_continuous(breaks = 1:10) + xlab("") + ylab("")  + geom_text(aes(x = 5.5, y = c(6.15,5.65,5.15), label = c("Normal transmission",'Temporarily interrupted transmission','Permanently interrupted transmission')), size = 6) + guides(color = F, shape = F) + theme_minimal() + theme(text = element_text(size = 16)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.title = element_text(size = rel(1))) + scale_shape_manual(values = c(15,16))


######
##  ##
######


###### 
## >> loading data for figures 3-5 & S1-S4
## set working directory to the Measures folder from SM
#setwd("~/Measures")

file_list = list.files()
dataset = list()

file_list

## reading the files and adding the condition information
for (i in c(10:12,16:18,26,30,31)){ 
  dataset[[file_list[i]]] = read.table(file_list[i], header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  dataset[[file_list[i]]]$file = file_list[i]
  dataset[[file_list[i]]][,1] = as.integer(as.character(dataset[[file_list[i]]][,1]))
  dataset[[file_list[i]]][,2] = as.integer(as.character(dataset[[file_list[i]]][,2]))
  dataset[[file_list[i]]][,3] = as.numeric(as.character(dataset[[file_list[i]]][,3]))
  if (grepl("_n.csv",file_list[i])) {
    dataset[[file_list[i]]]$cond = rep('L1',dim(dataset[[file_list[i]]])[1])
  } else if (grepl("_p.csv",file_list[i])) {
    dataset[[file_list[i]]]$cond = rep('L2L2',dim(dataset[[file_list[i]]])[1])
  } else {
    dataset[[file_list[i]]]$cond = rep('L2L1',dim(dataset[[file_list[i]]])[1])
  }
}



######
##  ##
######


######
## >> Figure 3. Change of type-token ratio over time.
ttr = rbind(dataset[[7]],dataset[[8]],dataset[[9]]) 
ttr_ddplot = ddply(ttr, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(ttr_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal() + scale_y_continuous('TTR',limits=c(0.260,0.375),breaks=seq(0.275,0.375,0.025)) + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16))


######
##  ##
######


######
## >> Figure 4. Change of the expressibility of agreement over time.
expr = rbind(dataset[[1]],dataset[[2]],dataset[[3]]) 
expr_ddplot = ddply(expr, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(expr_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal() + scale_y_continuous('Expressibility of agreement',limits=c(0,1)) + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16))

######
##  ##
######


######
## >> Figure 5. learnability over time. The y-axis starts from 0.8.
learn = rbind(dataset[[4]],dataset[[5]],dataset[[6]]) 
learn_ddplot = ddply(learn, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(learn_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(1,2,3,4,5,6,7,8,9,10), limits = c(1,10)) + theme_minimal() + scale_y_continuous('Learnability',limits=c(0.8,1)) + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16))


######
##  ##
######


######
## >> Figure 6. Learnability as a function of the expressibility of agreement. 
learn_expr = read.csv(file_list[19], header=F, sep=";", dec=".", col.names = c("chain","generation","learner","next_fidelity","overspec_vg"), skip=1) 
learn_expr$learner = ifelse(learn_expr$learner == "l1", "Normal learners", "Imperfect learners")
str(learn_expr)
learn_expr_ddplot = ddply(learn_expr, c("learner", "overspec_vg"), summarise, mean = mean(next_fidelity), se = se(next_fidelity))
str(learn_expr_ddplot)


ggplot() + geom_boxplot(data = learn_expr, aes(x = as.factor(round(overspec_vg,2)), y = next_fidelity)) + theme_minimal() + scale_y_continuous('Learnability', limits = c(-0.05,1),expand = c(0,0)) + scale_x_discrete("\nExpressibility of agreement", expand = c(0.01,0),limits = rev(levels(as.factor(round(learn_expr$overspec_vg,2))))) + facet_grid(~ learner) + theme(text = element_text(size = 16)) + theme(panel.margin = grid::unit(2.5, "lines"), legend.position = "bottom") + geom_point(data = learn_expr_ddplot, aes(x = as.factor(round(overspec_vg,2)), y = mean), size = 3, shape = 15) 


######
##  ##
######


###### 
## >> loading data for figures S1-S4
dataset_SM = list()

## reading the files and adding the condition information
for (i in c(1:15,27:29,32:37)){ 
  dataset_SM[[file_list[i]]] = read.table(file_list[i], header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  dataset_SM[[file_list[i]]]$file = file_list[i]
  dataset_SM[[file_list[i]]][,1] = as.integer(as.character(dataset_SM[[file_list[i]]][,1]))
  dataset_SM[[file_list[i]]][,2] = as.integer(as.character(dataset_SM[[file_list[i]]][,2]))
  dataset_SM[[file_list[i]]][,3] = as.numeric(as.character(dataset_SM[[file_list[i]]][,3]))
  if (grepl("_n.csv",file_list[i])) {
    dataset_SM[[file_list[i]]]$cond = rep('L1',dim(dataset_SM[[file_list[i]]])[1])
  } else if (grepl("_p.csv",file_list[i])) {
    dataset_SM[[file_list[i]]]$cond = rep('L2L2',dim(dataset_SM[[file_list[i]]])[1])
  } else {
    dataset_SM[[file_list[i]]]$cond = rep('L2L1',dim(dataset_SM[[file_list[i]]])[1])
  }
}



######
##  ##
######


######
## >> Figure S1. Results of the comprehension test (normalized). 
compr = rbind(dataset_SM[[1]],dataset_SM[[2]],dataset_SM[[3]]) 
compr_ddplot = ddply(compr, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(compr_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(1,2,3,4,5,6,7,8,9,10), limits = c(1,10)) + theme_minimal()  + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) + scale_y_continuous("Comprehension rate", limits = c(0.75,1))


######
##  ##
######


######
## >> Figure S2. Underspecification: the share of ambiguous signals in the language.
underspec = rbind(dataset_SM[[22]],dataset_SM[[23]],dataset_SM[[24]]) 
underspec_ddplot = ddply(underspec, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(underspec_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal()  + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) + scale_y_continuous("Underspecification", limits = c(0,0.225))


######
##  ##
######


######
## >> Figure S3. TTR calculated separately for nouns and verbs.
ttr_n = rbind(dataset_SM[[16]],dataset_SM[[17]],dataset_SM[[18]]) 
ttr_n$category = 'Nouns'
ttr_v = rbind(dataset_SM[[19]],dataset_SM[[20]],dataset_SM[[21]]) 
ttr_v$category = 'Verbs'
ttr_n_v = rbind(ttr_n,ttr_v)
ttr_n_v_ddplot = ddply(ttr_n_v, c('cond', 'generation', 'category'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(ttr_n_v_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal()  + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "left", title.hjust = 0.5)) + theme(text = element_text(size = 16), legend.position = "bottom") + facet_wrap( ~ category) + scale_y_continuous("TTR", limits = c(0.25,0.52)) 


######
##  ##
######


######
## >> Figure S4. Change of the expressibility of the four meaning categories over time.
expr_agent = rbind(dataset_SM[[4]],dataset_SM[[5]],dataset_SM[[6]]) 
expr_agent$category = 'Agent'
expr_number = rbind(dataset_SM[[7]],dataset_SM[[8]],dataset_SM[[9]])
expr_number$category = 'Number'
expr_event = rbind(dataset_SM[[13]],dataset_SM[[14]],dataset_SM[[15]]) 
expr_event$category = 'Event'
expr_agreement = rbind(dataset_SM[[10]],dataset_SM[[11]],dataset_SM[[12]])
expr_agreement$category = 'Agreement'
expr_4 = rbind(expr_agent,expr_number,expr_event,expr_agreement) 
str(expr_4)
expr_4$category = factor(expr_4$category, levels = c("Number","Agent","Agreement","Event"))

expr_4_ddplot = ddply(expr_4, c('cond', 'generation', 'category'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(expr_4_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal()  + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) + facet_wrap( ~ category, nrow = 2) + scale_y_continuous("Expressibility", limits = c(0,1)) 


######
##  ##
######

######
sessionInfo()
## R version 3.1.0 (2014-04-10)
## Platform: x86_64-apple-darwin13.1.0 (64-bit)
## 
## locale:
## [1] C
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] plyr_1.8.3    ggplot2_1.0.0
## 
## loaded via a namespace (and not attached):
## [1] MASS_7.3-31      Rcpp_0.12.1      colorspace_1.2-4 digest_0.6.9    
## [5] grid_3.1.0       gtable_0.1.2     labeling_0.3     magrittr_1.5    
## [9] munsell_0.4.2    proto_0.3-10     reshape2_1.4.1   scales_0.2.4    
## [13] stringi_0.5-5    stringr_1.0.0    tools_3.1.0