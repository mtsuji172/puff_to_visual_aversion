##############################
# load libaries
##############################
source('./init_female.R')
source('./init_b-v-b-p-b-p-v-b_female.R')
source('./basic.R')

plotdir = paste0(outdir, 'export/')

##################################
# Basic calc for one dir
##################################
# calc and save
batchcalc(gsub('export/','', outdir))

##################################
# Load results
##################################
# list dirs
mydirs = paste0(list.dirs(path=gsub('export/','', outdir), recursive=FALSE), '/GminusR/aligned/')
mydirs = mydirs[-grep('export', mydirs)]

# preprocessing
mydirs = mydirs[grep('ID', mydirs)]
mydirs = gsub('//','/', mydirs)

# load result
mydata = 
    lapply(1:length(mydirs), function(k){
       read.csv(paste0(mydirs[k], 'mydata.csv'))
    }) %>% bind_rows
mydata.bin = 
    lapply(1:length(mydirs), function(k){
       read.csv(paste0(mydirs[k], 'mydata.bin.csv'))
    }) %>% bind_rows
tmp = str_split_fixed(mydata.bin$ID, '_', 3)
mydata.bin$ID_base = paste(tmp[,1], tmp[,2], sep='_')
mydata.bin$TRIAL = gsub('00','',tmp[,3]) %>% as.numeric()
mydata.flyROImean = 
    lapply(1:length(mydirs), function(k){
       if(file.exists(paste0(mydirs[k], 'mydata.flyROImean.csv')))
           read.csv(paste0(mydirs[k], 'mydata.flyROImean.csv'))
    }) %>% bind_rows
tmp = mydata.flyROImean$ID_ROI %>% str_split_fixed('_',4)
mydata.flyROImean$ID = paste(tmp[,1], tmp[,2], sep='_')
change = 
    lapply(1:length(mydirs), function(k){
       if(file.exists(paste0(mydirs[k], 'change.csv')))
           read.csv(paste0(mydirs[k], 'change.csv'))
    }) %>% bind_rows
tmp = change$ID_ROI %>% str_split_fixed('_',4)
change$ID = paste(tmp[,1], tmp[,2], sep='_')

write.csv(change %>% subset(phase=='puff', select=c(precedingz, zscore)), paste0(plotdir, 'change_raw.csv'), row.names=F)

# get ID.posi of thresh from 0 to 1
ID.set = unique(mydata.flyROImean$ID)
posinega = 
    lapply(1:length(ID.set), function(k){
          subdata = subset(mydata.flyROImean, ID==ID.set[k])
          subdata$profile = ifelse(mean(subdata$zscore[subdata$phase%in%c('puff','puff.previsual')])>1, 'posi', 'nega')
          subdata
    }) %>% bind_rows
write.csv(posinega, paste0(path2data, 'jGCaMP7f_152Hz_b-v-b-p-b-p-v-b_female/export/posinega.csv'), row.names=F)

##################################
# mean intensity along time
##################################
ggplot(mydata.bin %>% subset(X>25 & X<50), aes(X, dF)) +
    labs(x='', y='') +
    geom_rect(data=puffwindow, mapping=aes(NULL, NULL, xmin=30, xmax=40, ymin=y1, ymax=y2), color='white', alpha=.2, fill='red') + 
    geom_smooth(stat='summary', fun.data=mean_cl_boot, se=T, col='black') +
    theme_classic() + theme(text=element_text(size=25))
ggsave(paste0(plotdir, 'FR_timecourse_1puff.png'), height=3, width=4, dpi=300)

ggplot(mydata.bin, aes(X, dF)) +
    labs(x='', y='') +
    geom_hline(yintercept = 0) +
    geom_rect(data=puffwindow, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='white', alpha=.2, fill='red') + 
    geom_rect(data=visualwindow, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='white', alpha=.2, fill='blue') + 
    geom_smooth(stat='summary', fun.data=mean_cl_boot, se=T, col='black') +
    theme_classic() + theme(text=element_text(size=25))
ggsave(paste0(plotdir, 'FR_timecourse.png'), height=3, width=5, dpi=300)


##################################
# change
##################################

# test normality
mydata.flyROImean %>%
    group_by(phase) %>%
    shapiro_test(zscore) %>%
    print %>%
    write.csv(paste0(plotdir, 'mydata.flyROImean_shapiro.csv'), row.names=F)
change %>%
    group_by(phase) %>%
    shapiro_test(precedingz) %>%
    print %>%
    write.csv(paste0(plotdir, 'change_precedingz_shapiro.csv'), row.names=F)

# plot (median, as distributions are non-normal
change.melt = reshape2::melt(change %>% subset(phase%in%c('puff','visual.base'), select=c(ID_ROI, phase,zscore,precedingz)), id.vars=c('ID_ROI','phase'), variable.name='type')
change.melt$type = factor(change.melt$type, levels=c('precedingz','zscore'))
ggplot(change.melt %>% subset(phase=='puff'), aes(type, value, color=type, fill=type)) + 
    theme_classic() +
    theme(text = element_text(size=20), legend.position='None') +
    scale_color_manual(values=c('precedingz'='black', 'zscore'='red')) +
    scale_fill_manual(values=c('precedingz'='black', 'zscore'='red')) +
    ylim(c(-5,5)) +
    geom_line(col='gray40', alpha=.2, aes(group=ID_ROI)) +
    stat_summary(fun.y=median, fun.ymin=median, fun.ymax=median, geom='crossbar', width=.7)
ggsave(paste0(plotdir, 'mydata.flyROImean_precedingframe-vs-puff.png'), height=5, width=2, dpi=300)

ggplot(change.melt %>% subset(phase=='visual.base'), aes(type, value, color=type, fill=type)) + 
    theme_classic() +
    theme(text = element_text(size=20), legend.position='None') +
    scale_color_manual(values=c('precedingz'='black', 'zscore'='blue')) +
    scale_fill_manual(values=c('precedingz'='black', 'zscore'='blue')) +
    ylim(c(-5,5)) +
    geom_line(col='gray40', alpha=.2, aes(group=ID_ROI)) +
    stat_summary(fun.y=median, fun.ymin=median, fun.ymax=median, geom='crossbar', width=.7)
ggsave(paste0(plotdir, 'mydata.flyROImean_precedingframe-vs-visual.base.png'), height=5, width=2, dpi=300)

# stat

# check normality
change %>%
    group_by(phase) %>%
    shapiro_test(zscore) %>% 
    print %>%
    write.csv(paste0(plotdir, 'change_shapiro.csv'), row.names=F)

change.visual.base = subset(change, phase=='visual.base')
mytest.visual.base = t.test(change.visual.base$zscore)
effsize.visual.base = with(change.visual.base, mean(zscore)/sd(zscore))
mytest.df.visual.base = with(mytest.visual.base, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t %>% as.data.frame
colnames(mytest.df.visual.base) = c('method','parameter','statistic','p.value')
mytest.df.visual.base$effsize = effsize.visual.base
mytest.df.visual.base$phase = 'visual.base'

change.puff = subset(change, phase=='puff')
mytest.puff = wilcox.test(change.puff$zscore)
mytest.puff[sapply(mytest.puff, is.null)] = NA
effsize.puff = wilcoxonOneSampleRC(change.puff$zscore, mu=0)
mytest.df.puff = with(mytest.puff, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t %>% as.data.frame
colnames(mytest.df.puff) = c('method','parameter','statistic','p.value')
mytest.df.puff$effsize = effsize.puff
mytest.df.puff$phase = 'puff'

mytest.df = rbind(mytest.df.visual.base, mytest.df.puff)
mytest.df$p.adjusted = mytest.df$p.value %>% p.adjust(method = 'bonferroni')
print(mytest.df)
write.csv(mytest.df, paste0(plotdir, 'change_stat.csv'), row.names=F)
