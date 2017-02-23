library(ggplot2)
library(plyr)
library(PSYC201)


######
## >> getting the data
setwd("~/SM/Analyzed")

file_list = list.files()
dataset = list()

file_list

## >> reading the files and adding the condition information
for (i in 1:45){ 
  print(i)
  dataset[[file_list[i]]] = read.table(file_list[i], header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  dataset[[file_list[i]]][,1] = as.integer(as.character(dataset[[file_list[i]]][,1]))
  dataset[[file_list[i]]][,2] = as.integer(as.character(dataset[[file_list[i]]][,2]))
  dataset[[file_list[i]]][,3] = as.numeric(as.character(dataset[[file_list[i]]][,3]))
  if (i %% 3 == 1) {
    dataset[[file_list[i]]]$cond = rep('L2L2',dim(dataset[[file_list[i]]])[1])
  } else if (i %% 3 == 2) {
    dataset[[file_list[i]]]$cond = rep('L2L1',dim(dataset[[file_list[i]]])[1])
  } else {
    dataset[[file_list[i]]]$cond = rep('L1',dim(dataset[[file_list[i]]])[1])
  }
}





######
## >> figures

## Figure 4. Change of the expressibility of the four meaning categories over time.
temp1 = rbind(dataset[[25]],dataset[[26]],dataset[[27]])
temp1$word = 'Number'
temp2 = rbind(dataset[[28]],dataset[[29]],dataset[[30]])
temp2$word = 'Agreement'
temp3 = rbind(temp1,temp2)
rm(temp1,temp2)
temp4 = ddply(temp3, c('cond', 'generation', 'word'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

temp1 = rbind(dataset[[22]],dataset[[23]],dataset[[24]])
temp1$word = 'Agent'
temp2 = rbind(dataset[[31]],dataset[[32]],dataset[[33]])
temp2$word = 'Event'
temp3b = rbind(temp1,temp2)
rm(temp1,temp2)
temp4b = ddply(temp3b, c('cond', 'generation', 'word'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

temp5 = rbind(temp4,temp4b)
expr_ddplot = temp5
rm(temp3,temp4,temp5)

expr_ddplot$word = as.factor(expr_ddplot$word)
expr_ddplot$word = factor(expr_ddplot$word, levels = c('Number','Agent','Agreement','Event'))

ggplot(expr_ddplot,aes(x=generation,y=mean,colour=cond,linetype=word,shape=cond)) + geom_point(size = 3) + geom_line() + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal() + scale_y_continuous('Expressibility',limits=c(0.17,1),breaks=seq(0.25,1,0.25)) + labs(colour = 'Transmission', linetype = '', shape = 'Transmission') + scale_colour_manual(values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual(values = c('solid','solid','solid','solid')) + scale_shape_manual(labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + theme(text = element_text(size = 16)) + facet_wrap(~ word, ncol=2) + guides(linetype = F)




## Figure 5. Change of TTR over time.
ttr = rbind(dataset[[34]],dataset[[35]],dataset[[36]])
ttr_ddplot = ddply(ttr, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(ttr_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond)) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal() + scale_y_continuous('TTR',limits=c(0.274,0.37),breaks=seq(0.275,0.375,0.025)) + labs(colour = 'Transmission', linetype = 'Transmission', shape = 'Transmission') + scale_colour_manual(values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual(labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual(labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16))



## Figure 6. Change of TTR over time, measured separately for nouns and verbs.
ttr_nouns = rbind(dataset[[37]],dataset[[38]],dataset[[39]])
ttr_nouns_ddplot = ddply(ttr_nouns, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))
ttr_verbs = rbind(dataset[[40]],dataset[[41]],dataset[[42]])
ttr_verbs_ddplot = ddply(ttr_verbs, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))
temp1 = ttr_nouns_ddplot
temp1$word = 'noun'
temp2 = ttr_verbs_ddplot
temp2$word = 'verb'
ttr_n_v_ddplot = rbind(temp1,temp2)
rm(temp1,temp2)

ggplot(ttr_n_v_ddplot,aes(x=generation,y=mean,colour=cond,linetype=word,shape=cond)) + geom_point(size = 5) + geom_line() + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal() + scale_y_continuous('TTR',limits=c(0.25,0.51),breaks=seq(0.25,0.5,0.05)) + labs(colour = 'Transmission', linetype = '', shape = 'Transmission') + scale_colour_manual(values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual(labels = c('Nouns','Verbs'), values = c('solid','dashed')) + scale_shape_manual(labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + theme(text = element_text(size = 16))


## Figure 7. Results of the comprehension test (normalized). 
comprehension_rate = rbind(dataset[[4]],dataset[[5]],dataset[[6]])
comprehension_rate_ddplot = ddply(comprehension_rate, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(comprehension_rate_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond)) + scale_x_continuous('Generation',breaks = c(1,2,3,4,5,6,7,8,9,10), limits = c(1,10)) + theme_minimal() + scale_y_continuous('Comprehension rate',limits=c(0.8,1),breaks=seq(0.8,1,0.05)) + labs(colour = 'Transmission', linetype = 'Transmission', shape = 'Transmission') + scale_colour_manual(values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual(labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual(labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) 



## Figure 8. Underspecification: the share of ambiguous signals in the language
underspecification = rbind(dataset[[43]],dataset[[44]],dataset[[45]])
underspecification_ddplot = ddply(underspecification, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(underspecification_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond)) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal() + scale_y_continuous('Underspecification',limits=c(0,0.16),breaks=seq(0,0.15,0.05)) + labs(colour = 'Transmission', linetype = 'Transmission', shape = 'Transmission') + scale_colour_manual(values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual(labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual(labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) 



## Figure 9. Change rate, measured as average pairwise normalized Levenshtein distance.
change_rate = rbind(dataset[[1]],dataset[[2]],dataset[[3]])
change_rate_ddplot = ddply(change_rate, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(change_rate_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond))  + scale_x_continuous('Generation',breaks = c(1,2,3,4,5,6,7,8,9,10), limits = c(1,10)) + theme_minimal() + scale_y_continuous('Change rate',limits=c(0,0.15),breaks=seq(0,0.15,0.05)) + labs(colour = 'Transmission', linetype = 'Transmission', shape = 'Transmission') + scale_colour_manual(values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual(labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual(labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) 
