
##################################
# packages and params common for all analyses
##################################

library('ggplot2')
library('reshape2')
library('stringr')
library('gplots')
library('dendextend')
library('dplyr')
library('ggrepel')
library('gridExtra')
library('corrr')
library('corrplot')
library('foreach')
library('doParallel')
library('dplR')
library('signal')
library('synchrony')
library('seewave')
library('tidyr')
library('stringr')
library('factoextra')
library('circlize')
library('WaveletComp')
library('e1071')
library('rcompanion')
library('effsize')
library('rstatix')

##################################
# general params
##################################
path2data ='/media/masato/CaImaging_1/file/data/'
ncore = detectCores()[1] - 2
sessionDuration = 80 #[s]
acceptedrange = .05
pretime = 0
mydt = 80 / 12000
fs = 1/mydt
targetdt = 0.5
puff.set = 1:8
myspan = NULL # smoothing factor for fft
cc = c('1'='gray','2'='skyblue','3'='black','4'='red','5'='dodgerblue','6'='gray')
jet.colors = colorRampPalette(c("darkblue", "blue", "deepskyblue3", "mediumseagreen", "orange", "yellow", "lemonchiffon"))
celltype.set = c('ctrl','cut','glued','asahina')
celltypecolors = c('ctrl'='black','cut'='blue', 'glued'='red')
freq.set = 2^(seq(-3,6.2, by=.1))
phase.set = c('base', 'visual.base', 'base.visual', 'puff', 'base.puff', 'puff.previsual', 'visual.puff', 'base.visual.postpuff')
livelihood = read.csv(paste0(path2data, 'livelihood.csv'))
mymargin = 3 # plot margin for "FOV"s to avoid peripheral nodes extending beyond the plot (happens when node size is set to a large value)

##################################
# params for across-dir analyses
##################################
outdirs = 
    paste0(path2data, 
           c(
            'jGCaMP7f_152Hz_visual5_b-v-b-p-b-p-v-b/export/',
            'jGCaMP7f_152Hz_visual5_b-p-v-b-p-b-v-b/export/'
            ))
myschedules = list(
                   list('base'=list(c(1,10)),
                          'visual.base'=list(c(10,20)),
                          'base.visual'=list(c(20,30)),
                          'puff'=list(c(30,40)),
                          'base.puff'=list(c(40,50)),
                          'puff.previsual'=list(c(50,60)),
                          'visual.puff'=list(c(60,70)),
                          'base.visual.postpuff'=list(c(70,80))),
                   list('base'=list(c(1,10)),
                        'visual.base'=list(c(60,70)),
                        'base.visual'=list(c(70,80)),
                        'puff'=list(c(40,50)),
                        'base.puff'=list(c(50,60)),
                        'puff.previsual'=list(c(10,20)),
                        'visual.puff'=list(c(20,30)),
                        'base.visual.postpuff'=list(c(30,40)))
                   )
posinega = rbind(
                read.csv(paste0(path2data, 'jGCaMP7f_152Hz_visual5_b-v-b-p-b-p-v-b/export/posinega.csv')),
                read.csv(paste0(path2data, 'jGCaMP7f_152Hz_visual5_b-p-v-b-p-b-v-b/export/posinega.csv'))
                )

schedule.date = data.frame(date=c('20210209','20210210','20210211','20210216','20210222','20210224'), schedule=c('F','F','F','R','R','R'))

# pick flies w/o sarine leakage
ID.sarineOK = with(livelihood,
                 paste0(date,'_ID',ID)[sarine=='ok'])
ID.posi = subset(posinega, ID%in%ID.sarineOK & profile=='posi')$ID %>% unique

# get ID.posi of thresh 1
posinega_ori = posinega
posinega_ori = subset(posinega_ori, ID%in%ID.sarineOK)
ID.set = unique(posinega_ori$ID)
posinega = 
    lapply(1:length(ID.set), function(k){
          subdata = subset(posinega_ori, ID==ID.set[k])
          subdata$profile = ifelse(mean(subdata$zscore[subdata$phase%in%c('puff','puff.previsual')])>1, 'posi', 'nega')
          subdata
    }) %>% bind_rows
ID.posi = subset(posinega, ID%in%ID.sarineOK & profile=='posi')$ID %>% unique
