library(reshape2)
library(ggplot2)
library(dplyr)
library(parallel)
library(doParallel)
library(e1071)
library(seewave)
library(stringr)
library(gridExtra)
library(wavelets)
library(DescTools)
source('./params.R')
source('./params/CS_for-aversiveshift-manuscript.R')
source('./functions.R')

###########################
# preprocess (per day)
###########################

# generate dFrames
gen_dPixels(root, overwrite=F)

# detect beats
detectBeats(root, overwrite=F)


##############################
# read mydata.dist data
##############################
# prep dir
plotdir = gsub('_mwt','',plotdir)
dir.create(plotdir, recursive=T, showWarnings=F)

# read files
myfiles = list.files(path=paste0(root,'dPixels_distance/'), pattern='.csv$')
myfiles = paste0(root,'dPixels_distance/', myfiles)

mydata.dist = 
    lapply(1:length(myfiles), function(k){
       mydata = read.csv(myfiles[k]) %>% select(-any_of(c('fnum','ID')))
       srate = nrow(mydata) / trialduration
       bf.cont = bf.cont = signal::butter(2, W=1/(srate/2), type='high')
       yDistance.diff = signal::filter(bf.cont, mydata$yDistance.diff)
       yDistance.estimate = cumsum(yDistance.diff)
       yDistance.estimate.z = (yDistance.estimate - mean(yDistance.estimate)) / sqrt(var(yDistance.estimate))
       mydata$yDistance.estimate = yDistance.estimate
       mydata
    }) %>% bind_rows


# load manual labels for diagnostics
source('../code_compare_codes/functions.R')
mydata.dist.manual = loadData_manual(minframe=240, maxframe=3009)
uniq_trial.set = intersect(unique(mydata.dist$uniq_trial),
                           unique(mydata.dist.manual$uniq_trial))

mydata.dist.merge = 
    lapply(1:length(uniq_trial.set), function(u){
        out = merge(subset(mydata.dist, uniq_trial==uniq_trial.set[u]),
                    subset(mydata.dist.manual, uniq_trial==uniq_trial.set[u], select=-uniq_trial),
                    by.x = 'frame')
        out
    }) %>% bind_rows
mydata.dist.merge$contraction.peak[mydata.dist.merge$peak.manual==1] = mydata.dist.merge$contraction.peak[mydata.dist.merge$peak.manual==1] + 0.5
mydata.dist = mydata.dist.merge
rm(mydata.dist.merge)

# plot
print(unique(mydata.dist$DATE))
ggplot(subset(mydata.dist, DATE=='20220811' & time>=55 & time<75), aes(time, yDistance.estimate, color=as.factor(contraction.peak))) +
    facet_wrap(GENOTYPE~uniq_trial, ncol=2) +
    theme_classic() +
    theme(legend.position='None') +
    geom_rect(data=puffwindow, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='white', alpha=.25, fill='red') +
    scale_x_continuous(breaks = round(seq(min(mydata.dist$frame), max(mydata.dist$frame), by=5),1)) +
    scale_color_manual(values=c('0'='gray','0.5'='skyblue','1'='red','1.5'='black')) +
    scale_alpha_manual(values=c('0'=0,'1'=1)) +
    geom_line(col='black') +
    geom_point(aes(alpha=as.factor(contraction.peak)))
ggsave(paste0(plotdir, 'yDistance.estimate_demo_20210419_2_0.png'), height=2, width=10, dpi=300)

##########################
# HR timecourse (1s-bin)
##########################
HR.timecourse = get_timecourse(subset(mydata.dist) , zscore=F)
ggplot(subset(HR.timecourse, time>=30 & time<90), aes(time, HR)) +
    facet_wrap(.~GENOTYPE, nrow=1) +
    theme_classic() +
    theme(text=element_text(size=20), legend.position='None') +
    labs(y='HR (beats/s)', x='time bin (s)') +
    scale_color_manual(values=mycolors) +
    scale_fill_manual(values=mycolors) +
    geom_rect(data=puffwindow, mapping=aes(NULL, NULL, xmin=60, xmax=70, ymin=y1, ymax=y2), color='white', alpha=.2, fill='red') + 
    geom_smooth(stat='summary', fun.data=mean_cl_boot, aes(group=GENOTYPE, color=GENOTYPE))
ggsave(paste0(plotdir, 'HR_timecourse.png'), height=5, width= 1 + 4*length(genotype.set), dpi=300, limitsize=F)


##########################
# proceed with different puff windows
##########################

### get HR within specified time windows
HR.windowave = rbind(
                      data.frame(get_windowave(mydata.dist, base.window), bin='base'),
                      data.frame(get_windowave(mydata.dist, puff.window), bin='puff')
                      )
HR.windowave = na.omit(HR.windowave)
ggplot(HR.windowave, aes(as.factor(bin), HR, color=as.factor(bin), fill=as.factor(bin))) +
    facet_grid(.~GENOTYPE) +
    scale_color_manual(values=c('base'='black','puff'='red')) +
    scale_fill_manual(values=c('base'='black','puff'='red')) +
    theme_classic() +
    theme(legend.position="None", text=element_text(size=20)) +
    labs(y='HR', x='time bin (s)') +
    geom_boxplot(alpha=.3, lwd=1) +
    geom_line(aes(group=uniq_fly), alpha=.5, col='black')
ggsave(paste0(plotdir,'HR.windowave_',puff.windows[[p]][1],'-',puff.windows[[p]][2],'.png'), height=5, width=3+1*length(genotype.set), dpi=300)
write.csv(HR.windowave, '../data/export/HR.windowave.csv', row.names=F)

# get fly-wise change
uniq_fly.set = unique(HR.windowave$uniq_fly)
HR.windowave.diff = 
   lapply(1:length(uniq_fly.set), function(k){
              subdata = subset(HR.windowave, uniq_fly==uniq_fly.set[k])
              if(nrow(subdata)==2){
                  out = subset(subdata[1,], select=-c(bin))
                  out$HR = with(subdata, (HR[bin=='puff'] / HR[bin=='base'] - 1) * 100)
                  out
              }
   }) %>% bind_rows
HR.windowave.diff.melt = reshape2::melt(HR.windowave.diff, id.vars=colnames(HR.windowave.diff)[colnames(HR.windowave.diff)!='HR'], variable.name='timing', value.name='HR')
ggplot(HR.windowave.diff.melt, aes(GENOTYPE, HR, color=GENOTYPE, fill=GENOTYPE)) +
    theme_classic() +
    theme(legend.position="None", text=element_text(size=20), axis.text.x = element_text(angle = 30, hjust=1)) +
    scale_color_manual(values=mycolors) +
    scale_fill_manual(values=mycolors) +
    labs(y='⊿ HR (%)', x='') +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(alpha=.3, outlier.shape=NA, lwd=1) +
    geom_dotplot(binaxis = "y", stackdir = "center", alpha=1, dotsize=.5)
ggsave(paste0(plotdir,'HR.windowave.diff.png'), height=6, width=1.5+.5*length(unique(HR.windowave.diff$GENOTYPE)), dpi=300)

# stat
# n size
nsize = summary(as.factor(HR.windowave.diff$GENOTYPE)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0(plotdir, 'nsize.csv'), row.names=T)

# one-sample t-test
pvalue = t.test(HR.windowave.diff$HR)$p.value
print(pvalue)
write.csv(pvalue %>% as.data.frame, paste0(plotdir,'HR.diff_stat.csv'), row.names=T)
