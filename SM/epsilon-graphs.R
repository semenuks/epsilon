library(ggplot2)
library(plyr)
library(PSYC201)

###### 
## >> set to your Analyzed folder from SM
setwd("~/Analyzed")

file_list = list.files()
dataset = list()

file_list

## >> reading the files and adding the condition information
for (i in c(22:36,4:6,1:3)){ 
  print(i)
  dataset[[file_list[i]]] = read.table(file_list[i], header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  dataset[[file_list[i]]]$file = file_list[i]
  dataset[[file_list[i]]][,1] = as.integer(as.character(dataset[[file_list[i]]][,1]))
  dataset[[file_list[i]]][,2] = as.integer(as.character(dataset[[file_list[i]]][,2]))
  dataset[[file_list[i]]][,3] = as.numeric(as.character(dataset[[file_list[i]]][,3]))
  if (i %% 3 == 1) {
    dataset[[file_list[i]]]$cond = rep('L1',dim(dataset[[file_list[i]]])[1])
  } else if (i %% 3 == 2) {
    dataset[[file_list[i]]]$cond = rep('L2L2',dim(dataset[[file_list[i]]])[1])
  } else {
    dataset[[file_list[i]]]$cond = rep('L2L1',dim(dataset[[file_list[i]]])[1])
  }
}


######
## >> figures 

## Figure 4. Change of TTR over time.
ttr = rbind(dataset[[13]],dataset[[14]],dataset[[15]]) 
ttr_ddplot = ddply(ttr, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(ttr_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal() + scale_y_continuous('TTR',limits=c(0.260,0.375),breaks=seq(0.275,0.375,0.025)) + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16))



## Figure 6. Change of the expressibility of the four meaning categories over time.
temp1 = rbind(dataset[[4]],dataset[[5]],dataset[[6]])
temp1$word = 'Number'
temp2 = rbind(dataset[[7]],dataset[[8]],dataset[[9]])
temp2$word = 'Agreement'
temp3 = rbind(temp1,temp2)
rm(temp1,temp2)
temp4 = ddply(temp3, c('cond', 'generation', 'word'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

temp1 = rbind(dataset[[1]],dataset[[2]],dataset[[3]])
temp1$word = 'Agent'
temp2 = rbind(dataset[[10]],dataset[[11]],dataset[[12]])
temp2$word = 'Event'
temp3b = rbind(temp1,temp2)
rm(temp1,temp2)
temp4b = ddply(temp3b, c('cond', 'generation', 'word'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

temp5 = rbind(temp4,temp4b)
expr_ddplot = temp5
rm(temp3,temp3b,temp4,temp4b,temp5)

expr_ddplot$word = as.factor(expr_ddplot$word)
expr_ddplot$word = factor(expr_ddplot$word, levels = c('Number','Agent','Agreement','Event'))

ggplot(expr_ddplot,aes(x=generation,y=mean,shape=cond)) + geom_point(size = 3,aes(colour=cond)) + geom_line(aes(colour=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), size = 0, alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal() + scale_y_continuous('Expressibility') + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', values = c('solid','solid','solid','solid')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + theme(text = element_text(size = 16)) + facet_wrap(~ word, ncol=2) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) 



## Figure 9. Results of the comprehension test (normalized). 
comprehension_rate = rbind(dataset[[16]],dataset[[17]],dataset[[18]])
comprehension_rate_ddplot = ddply(comprehension_rate, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(comprehension_rate_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond,shape=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(1,2,3,4,5,6,7,8,9,10), limits = c(1,10)) + theme_minimal() + scale_y_continuous('Comprehension rate',limits=c(0,1)) + labs(colour = 'Transmission', linetype = 'Transmission', shape = 'Transmission') + scale_colour_manual(values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual(labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual(labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) 



## Figure 11. Transmission error at generation 2 for two learner types.
change_rate = rbind(dataset[[19]],dataset[[20]],dataset[[21]])
change_rate = change_rate[change_rate$generation == 2,]
change_rate$learnertype = as.factor(ifelse(change_rate$cond == 'L1',"L1","L2"))
levels(change_rate$learnertype) = c('Normal learner','Imperfect learner')

ggplot(change_rate,aes(x=learnertype,y=mvalue)) + geom_boxplot() + theme_minimal() + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) + xlab("") + ylab("Transmission error")
