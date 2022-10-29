
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

##################################
# general params
##################################
ncore = detectCores()[1] - 2
sessionDuration = 80 #[s]
acceptedrange = .05
pretime = 0
mydt = 80 / 12000
fs = 1/mydt
targetdt = 0.1
puff.set = 1:8
myspan = NULL # smoothing factor for fft
cc = c('1'='gray','2'='skyblue','3'='black','4'='red','5'='dodgerblue','6'='gray')
jet.colors = colorRampPalette(c("darkblue", "blue", "deepskyblue3", "mediumseagreen", "orange", "yellow", "lemonchiffon"))
celltype.set = c('ctrl','cut','glued','asahina')
celltypecolors = c('ctrl'='black','cut'='blue', 'glued'='red', 'asahina'='green')
freq.set = 2^(seq(-3,6.2, by=.1))
phase.set = c('base', 'visual.base', 'base.visual', 'puff', 'base.puff', 'puff.previsual', 'visual.puff', 'base.visual.postpuff')
livelihood = read.csv('../../data/livelihood.csv')
mymargin = 3 # plot margin for "FOV"s to avoid peripheral nodes extending beyond the plot (happens when node size is set to a large value)

##################################
# params for across-dir analyses
##################################
outdirs = c(
            '../../data/jGCaMP7f_152Hz_visual5_b-v-b-p-b-p-v-b/export/',
            '../../data/jGCaMP7f_152Hz_visual5_b-p-v-b-p-b-v-b/export/'
            )
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
                read.csv('../../data/jGCaMP7f_152Hz_visual5_b-v-b-p-b-p-v-b/export/posinega.csv'),
                read.csv('../../data/jGCaMP7f_152Hz_visual5_b-p-v-b-p-b-v-b/export/posinega.csv')
                )

# pick flies w/o sarine leakage
ID.sarineOK = with(livelihood,
                 paste0(date,'_ID',ID)[sarine=='ok'])
ID.posi = subset(posinega, ID%in%ID.sarineOK & profile=='posi')$ID %>% unique
