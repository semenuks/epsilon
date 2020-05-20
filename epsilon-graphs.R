## >> loading up the packages; se function; session info
######
library(ggplot2)
library(plyr)

se = function(x, na.rm = FALSE) {
  n = ifelse(na.rm, sum(!is.na(x)), length(x))
  return(sd(x, na.rm)/sqrt(n))
}

sessionInfo()

## R version 3.5.3 (2019-03-11)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS Mojave 10.14.6
## 
## Matrix products: default
## BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] plyr_1.8.4    ggplot2_3.1.0
## 
## loaded via a namespace (and not attached):
## [1] Rcpp_1.0.1       withr_2.1.2      assertthat_0.2.1 dplyr_0.8.1      crayon_1.3.4     R6_2.4.0         grid_3.5.3      
## [8] gtable_0.2.0     magrittr_1.5     scales_1.0.0     pillar_1.3.1     rlang_0.3.4      lazyeval_0.2.2   rstudioapi_0.10 
## [15] tools_3.5.3      glue_1.3.1       purrr_0.3.2      munsell_0.5.0    yaml_2.2.0       compiler_3.5.3   pkgconfig_2.0.2
## [22] colorspace_1.4-1 tidyselect_0.2.5 tibble_2.1.1    

######

######
##  ##
######


## >> Figure 1. A schematic representation of the iterated learning model in the experiment.
######
fig1.df.points = data.frame(y = rep(c(6,5.5,5),each=10), x = rep(1:10,3), cond = rep(c('L1','L2L1','L2L2'),each=10), label = c(rep("L",10),"L","S","S","S",rep("L",6),"L",rep("S",9)))
fig1.df.arrows = data.frame(y = rep(c(6,5.5,5),each=11), yend = rep(c(6,5.5,5),each=11), x = (rep(1:11,3)-0.65), xend = (rep(1:11,3)-0.35))

fig1.df.points$label = factor(fig1.df.points$label, levels = c("S","L"))

ggplot() + geom_point(data = fig1.df.points, aes(x=x, y=y, color = label, shape = label), size = 12) + geom_text(data = fig1.df.points, aes(x=x+0.015, y=y, label = label), color = "white", fontface="bold") + geom_segment(data = fig1.df.arrows, aes(x = x, xend = xend, y = y, yend = yend), arrow = grid::arrow(length = grid::unit(0.3,"cm"))) + scale_y_continuous(limits = c(4.90,6.25)) + scale_x_continuous(breaks = 1:10) + xlab("") + ylab("")  + geom_text(aes(x = 5.5, y = c(6.15,5.65,5.15), label = c("Normal transmission",'Temporarily interrupted transmission','Permanently interrupted transmission')), size = 6) + guides(color = F, shape = F) + theme_minimal() + theme(text = element_text(size = 16)) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.title = element_text(size = rel(1))) + scale_shape_manual(values = c(15,16))

######

######
##  ##
######


## >> moving to the folder with the data for the plots; reading file names; function for assigning condition based on file name
######

## set working directory to the Measures folder from SM
#setwd("/SM/Measures")

file_list = list.files()

## function for assigning condition based on file name
giveCondition = function(file_name) {
  if (grepl('_n.csv',file_name,fixed=T)) {
    return('L1')
  } else if (grepl('_t.csv',file_name,fixed=T)) {
    return('L2L1')
  } else {
    return('L2L2')
  }
}

######

######
##  ##
######


## >> Figure 3. Transmission fidelity at generation 2. 
######

temp_files = file_list[grep('fidelity',file_list)]

for (i in temp_files) {
  temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  temp$cond = giveCondition(i)
  if ('fidelity' %in% ls()) {
    fidelity = rbind(fidelity,temp)
  } else {
    fidelity = temp
  }
}
fidelity[,1] = as.integer(as.character(fidelity[,1]))
fidelity[,2] = as.integer(as.character(fidelity[,2]))
fidelity[,3] = as.numeric(as.character(fidelity[,3]))
fidelity$learnertype = ifelse(fidelity$cond == 'L1', 'Long-time learners', 'Short-time learners')

ggplot(fidelity,aes(x=learnertype,y=mvalue)) + geom_boxplot() + theme_minimal() + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) + xlab("") + ylab("Transmission fidelity")

######

######
##  ##
######


## >> Figure 4. Change of type-token ratio over time.
## >> Figure 5. Type-token ratio at generation 2
######
temp_files = file_list[grep('ttr_.\\.csv',file_list)]

for (i in temp_files) {
  temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  temp$cond = giveCondition(i)
  if ('ttr' %in% ls()) {
    ttr = rbind(ttr,temp)
  } else {
    ttr = temp
  }
}
ttr[,1] = as.integer(as.character(ttr[,1]))
ttr[,2] = as.integer(as.character(ttr[,2]))
ttr[,3] = as.numeric(as.character(ttr[,3]))

ttr_ddplot = ddply(ttr, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

## figure 4
ggplot(ttr_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal() + scale_y_continuous('TTR',limits=c(0.260,0.375),breaks=seq(0.275,0.375,0.025)) + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16))

## figure 5
ttr_gen2 = subset(ttr, generation == 2)
ttr_gen2$learnertype = ifelse(ttr_gen2$cond == 'L1', 'Long-time learners', 'Short-time learners')
ggplot(ttr_gen2,aes(x=learnertype,y=mvalue)) + geom_boxplot() + theme_minimal() + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) + xlab("") + ylab("TTR")

######

######
##  ##
######


## >> Figure 6. Change of type-token ratio over time separately for nouns and verbs.
######
temp_files = file_list[grep('ttr_nouns',file_list)]

for (i in temp_files) {
  temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  temp$cond = giveCondition(i)
  if ('ttr_n' %in% ls()) {
    ttr_n = rbind(ttr_n,temp)
  } else {
    ttr_n = temp
  }
}
ttr_n[,1] = as.integer(as.character(ttr_n[,1]))
ttr_n[,2] = as.integer(as.character(ttr_n[,2]))
ttr_n[,3] = as.numeric(as.character(ttr_n[,3]))
ttr_n$category = 'Nouns'


temp_files = file_list[grep('ttr_verbs',file_list)]

for (i in temp_files) {
  temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  temp$cond = giveCondition(i)
  if ('ttr_v' %in% ls()) {
    ttr_v = rbind(ttr_v,temp)
  } else {
    ttr_v = temp
  }
}
ttr_v[,1] = as.integer(as.character(ttr_v[,1]))
ttr_v[,2] = as.integer(as.character(ttr_v[,2]))
ttr_v[,3] = as.numeric(as.character(ttr_v[,3]))
ttr_v$category = 'Verbs'

ttr_n_v = rbind(ttr_n,ttr_v)
ttr_n_v_ddplot = ddply(ttr_n_v, c('cond', 'generation', 'category'), summarise, mean = mean(mvalue), se = se(mvalue))

ggplot(ttr_n_v_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal()  + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "left", title.hjust = 0.5)) + theme(text = element_text(size = 16), legend.position = "bottom") + facet_wrap( ~ category) + scale_y_continuous("TTR", limits = c(0.25,0.52)) 

######

######
##  ##
######


## >> Figure 7. Change of type-token ratio over time separately for verb stems and verb endings.
######

temp_files = file_list[grep('ttr_v_stem',file_list)]

for (i in temp_files) {
  temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  temp$cond = giveCondition(i)
  if ('ttr_v_stem' %in% ls()) {
    ttr_v_stem = rbind(ttr_v_stem,temp)
  } else {
    ttr_v_stem = temp
  }
}
ttr_v_stem[,1] = as.integer(as.character(ttr_v_stem[,1]))
ttr_v_stem[,2] = as.integer(as.character(ttr_v_stem[,2]))
ttr_v_stem[,3] = as.numeric(as.character(ttr_v_stem[,3]))
ttr_v_stem$category = 'Verb stems'


temp_files = file_list[grep('ttr_v_affix',file_list)]

for (i in temp_files) {
  temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  temp$cond = giveCondition(i)
  if ('ttr_v_affix' %in% ls()) {
    ttr_v_affix = rbind(ttr_v_affix,temp)
  } else {
    ttr_v_affix = temp
  }
}
ttr_v_affix[,1] = as.integer(as.character(ttr_v_affix[,1]))
ttr_v_affix[,2] = as.integer(as.character(ttr_v_affix[,2]))
ttr_v_affix[,3] = as.numeric(as.character(ttr_v_affix[,3]))
ttr_v_affix$category = 'Verb endings'

ttr_v_stem_v_affix = rbind(ttr_v_stem,ttr_v_affix)
ttr_v_stem_v_affix_ddplot = ddply(ttr_v_stem_v_affix, c('cond', 'generation', 'category'), summarise, mean = mean(mvalue), se = se(mvalue))
ttr_v_stem_v_affix_ddplot$category = factor(ttr_v_stem_v_affix_ddplot$category, levels = c('Verb stems','Verb endings'))


ggplot(ttr_v_stem_v_affix_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal()  + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "left", title.hjust = 0.5)) + theme(text = element_text(size = 16), legend.position = "bottom") + facet_wrap( ~ category) + scale_y_continuous("TTR", limits = c(0.29,0.53)) 

######


## >> Figure 8. Causal graph summarizing the claims of the paper.
######

causal_graph = read.csv('CausalLinksEpsilon.csv')

causal_graph_gg_points = data.frame(Var1 = unique(c(as.character(causal_graph$Var1),as.character(causal_graph$Var2))), labels = c("adult\nlanguage learners","\nimperfect learning", "\naccumulation of mutations","\nredundancy","\nlong-distance dependency","morphological complexity:\noverspecification"), x = c(0,1,1,1,1,2), y = c(4,4,3,2,1,2.5))

causal_graph_gg_arrows = join(causal_graph_gg_points,causal_graph[,c('Var1','Var2','Cor','Confirmed')], by = 'Var1')
temp = causal_graph_gg_points[,c(1,3,4)]
colnames(temp) = c('Var2','x2','y2')
causal_graph_gg_arrows = join(causal_graph_gg_arrows,temp, by = 'Var2')
causal_graph_gg_arrows = causal_graph_gg_arrows[!is.na(causal_graph_gg_arrows$x2),]
causal_graph_gg_arrows$labels = NULL
colnames(causal_graph_gg_arrows)[5] = 'Relationship'
causal_graph_gg_arrows$Relationship = ifelse(causal_graph_gg_arrows$Relationship == 'neg','Negative','Positive')
causal_graph_gg_arrows$Confirmed = factor(ifelse(causal_graph_gg_arrows$Confirmed == 'yes','yes','no'), levels = c('yes','no'))


ggplot() + geom_point(data = causal_graph_gg_points, aes(x = x, y = y-0.05), size = 10) + geom_segment(data = causal_graph_gg_arrows, aes(x = x+0.10, xend = x2-0.10, y = y-0.05, yend = y2-0.05, color = Relationship, linetype = Confirmed), arrow = grid::arrow(length = grid::unit(0.6,"cm"), type = 'closed')) + geom_text(data = causal_graph_gg_points, aes(x = x, y = y+0.35, label = labels), size = 6) + theme_minimal() + theme(text = element_text(size = 16), legend.position = "bottom", axis.text = element_blank(), axis.title = element_blank(), axis.line = element_blank(), panel.grid = element_blank()) + scale_x_continuous("", limits = c(-0.5,2.5)) + scale_y_continuous("", limits = c(0.85,4.50)) + scale_colour_manual(values = c('red','purple')) + scale_linetype_manual(values = c('solid','dotted'))

######

######
##  ##
######


## >> Figure S1. Results of the comprehension test (normalized).
######
temp_files = file_list[grep('comprehension_rate',file_list)]

for (i in temp_files) {
  temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  temp$cond = giveCondition(i)
  if ('compr' %in% ls()) {
    compr = rbind(compr,temp)
  } else {
    compr = temp
  }
}
compr[,1] = as.integer(as.character(compr[,1]))
compr[,2] = as.integer(as.character(compr[,2]))
compr[,3] = as.numeric(as.character(compr[,3]))

compr_ddplot = ddply(compr, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(compr_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(1,2,3,4,5,6,7,8,9,10), limits = c(1,10)) + theme_minimal()  + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) + scale_y_continuous("Comprehension rate", limits = c(0.75,1))
######

######
##  ##
######


## >> Figure S2. Underspecification: the share of ambiguous signals in the language.
######
temp_files = file_list[grep('underspecification',file_list)]

for (i in temp_files) {
  temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  temp$cond = giveCondition(i)
  if ('underspec' %in% ls()) {
    underspec = rbind(underspec,temp)
  } else {
    underspec = temp
  }
}
underspec[,1] = as.integer(as.character(underspec[,1]))
underspec[,2] = as.integer(as.character(underspec[,2]))
underspec[,3] = as.numeric(as.character(underspec[,3]))

underspec_ddplot = ddply(underspec, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(underspec_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal()  + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) + scale_y_continuous("Comprehension rate", limits = c(0,0.25))

######

######
##  ##
######


## >> Figure S3. Change of the expressibility of the four categories over time.
######
temp_files = file_list[grep('expr_noun_lex',file_list)]

for (i in temp_files) {
  temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  temp$cond = giveCondition(i)
  if ('expr_agent' %in% ls()) {
    expr_agent = rbind(expr_agent,temp)
  } else {
    expr_agent = temp
  }
}
expr_agent[,1] = as.integer(as.character(expr_agent[,1]))
expr_agent[,2] = as.integer(as.character(expr_agent[,2]))
expr_agent[,3] = as.numeric(as.character(expr_agent[,3]))
expr_agent$category = 'Agent'


temp_files = file_list[grep('expr_noun_number',file_list)]

for (i in temp_files) {
  temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  temp$cond = giveCondition(i)
  if ('expr_number' %in% ls()) {
    expr_number = rbind(expr_number,temp)
  } else {
    expr_number = temp
  }
}
expr_number[,1] = as.integer(as.character(expr_number[,1]))
expr_number[,2] = as.integer(as.character(expr_number[,2]))
expr_number[,3] = as.numeric(as.character(expr_number[,3]))
expr_number$category = 'Number'


temp_files = file_list[grep('expr_verb_lex',file_list)]

for (i in temp_files) {
  temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  temp$cond = giveCondition(i)
  if ('expr_event' %in% ls()) {
    expr_event = rbind(expr_event,temp)
  } else {
    expr_event = temp
  }
}
expr_event[,1] = as.integer(as.character(expr_event[,1]))
expr_event[,2] = as.integer(as.character(expr_event[,2]))
expr_event[,3] = as.numeric(as.character(expr_event[,3]))
expr_event$category = 'Event'


temp_files = file_list[grep('[[:digit:]]_expr_verb_gender',file_list)]

for (i in temp_files) {
  temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
  temp$cond = giveCondition(i)
  if ('expr_agreement' %in% ls()) {
    expr_agreement = rbind(expr_agreement,temp)
  } else {
    expr_agreement = temp
  }
}
expr_agreement[,1] = as.integer(as.character(expr_agreement[,1]))
expr_agreement[,2] = as.integer(as.character(expr_agreement[,2]))
expr_agreement[,3] = as.numeric(as.character(expr_agreement[,3]))
expr_agreement$category = 'Agreement'

expr_4 = rbind(expr_agent,expr_number,expr_event,expr_agreement) 
expr_4$category = factor(expr_4$category, levels = c("Number","Agent","Agreement","Event"))

expr_4_ddplot = ddply(expr_4, c('cond', 'generation', 'category'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(expr_4_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal()  + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) + facet_wrap( ~ category, nrow = 2) + scale_y_continuous("Expressibility", limits = c(0,1)) 

######

######
##  ##
######


## >> Figure S4. Change of transmission fidelity over time.
######
temp_files = file_list[grep('fidelity',file_list)]

if (! ('fidelity' %in% ls())) {
  for (i in temp_files) {
    temp = read.table(i, header=F, sep=";", dec=".", col.names = c("chain","generation","mvalue"), skip=1)
    temp$cond = giveCondition(i)
    if ('fidelity' %in% ls()) {
      fidelity = rbind(fidelity,temp)
    } else {
      fidelity = temp
    }
  }
  fidelity[,1] = as.integer(as.character(fidelity[,1]))
  fidelity[,2] = as.integer(as.character(fidelity[,2]))
  fidelity[,3] = as.numeric(as.character(fidelity[,3]))
  
}

fidelity_ddplot = ddply(fidelity, c('cond', 'generation'), summarise, mean = mean(mvalue), se = se(mvalue), sd = sd(mvalue))

ggplot(fidelity_ddplot,aes(x=generation,y=mean)) + geom_point(size = 5,aes(colour=cond,shape=cond)) + geom_line(aes(colour=cond)) + geom_ribbon(aes(ymax = mean+se, ymin = mean-se, fill=cond), alpha = 0.1) + scale_x_continuous('Generation',breaks = c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10)) + theme_minimal()  + scale_colour_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + scale_linetype_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c('solid','dashed','dotted')) + scale_shape_manual('Transmission', labels = c('Normal','Temporarily interrupted','Permanently interrupted'), values = c(16,17,15)) + scale_fill_manual('Transmission', values = c('blue','darkgreen','red'), labels = c('Normal','Temporarily interrupted','Permanently interrupted')) + guides(colour = guide_legend(label.position = "top", title.hjust = 0.5)) + theme(text = element_text(size = 16)) + scale_y_continuous("Transmission fidelity")

######

