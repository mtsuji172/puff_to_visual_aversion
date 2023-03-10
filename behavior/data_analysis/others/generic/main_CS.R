source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

root = '/media/masato/free/file/behavior/data/'
mydir = 'CS/'
dir.create(mydir, showWarnings=F)

# import data
import(paste0(root,mydir,'export/'))

# pose estimation
loadData(paste0(root,mydir,'predictions/'), paste0(root,mydir,'statistics/'))

# calc indices for all files
main_batch(paste0(root,mydir,'ballrecordings/'), parallel=T)

# export data
dir.create(paste0(root,mydir,'export/'), showWarnings=F)
export(paste0(root,mydir,'export/'))


# plot object position along time
uniq.myset.demo = 'CS_6_2021.09.08'
ggplot(subset(mydf_all, trialtype=='normal' t<=5 & uniq==uniq.myset.demo), aes(t-.1, abs(objectAngularPosition))) +
    geom_rect(data=mydf, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=mycol), color='white', alpha=.2) +
    scale_x_continuous(expand = c(0, 0), trans='reverse') +
    scale_fill_manual(values=c('attraction'='blue', 'aversion'='red')) +
    geom_hline(yintercept=60, col='cyan3') +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, alpha=.6, lwd=2, se=T, aes(color=as.factor(npuff))) +
    scale_color_manual(values=c('black','red')) +
    coord_flip() +
    theme_classic() +
    theme(text=element_text(size=20), legend.position='None') +
    labs(x='', y='')

# plot timecourse of activity
mydf_all$genotype = factor(mydf_all$genotype, levels=rev(unique(mydf_all$genotype)))
mydf_all$npuff = as.factor(mydf_all$npuff)
ggplot(subset(mydf_all, t>1 & t<60), aes(t, sqrt(dx^2+dy^2), color=npuff, fill=npuff)) +
    theme_classic() +
    scale_y_continuous(expand=c(0,0)) +
    expand_limits(y=0) +
    scale_color_manual(values=c('black','red')) +
    scale_fill_manual(values=c('black','red')) +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, se=T, size=1)

### plot aversion index against trial#
ggplot(subset(result_all, uniq%in%unique(puffeffect$uniq)), aes(trial, index_onset, color=as.factor(npuff), fill=as.factor(npuff))) +
    theme_classic() +
    geom_hline(yintercept=0, lty=2) +
    scale_color_manual(values=c('0'='black', '10'='red')) +
    scale_fill_manual(values=c('0'='black', '10'='red')) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6) +
    geom_smooth(method='lm', se=F)
ggsave(paste0(mydir, '/result_all_trialeffect.png'), height=3, width=5, dpi=300)

write.csv(subset(result_all, uniq%in%unique(puffeffect$uniq), select=c(uniq, trial, npuff, index_onset)), paste0(mydir, 'result_all_raw.csv'), row.names=F)

# stat (correlation among trials)
mynpuff = 10 # or 0
mycor = 
    with(subset(result_all, uniq%in%unique(puffeffect$uniq) & npuff==mynpuff),
         cor.test(index_onset, trial))
print(mycor) %>% capture.output %>% write.table(paste0(mydir, '/trialeffect_corstat_puff',mynpuff,'.txt'), row.names=F, col.names=F)

###  aversion index
result_all_mean$npuff = as.factor(result_all_mean$npuff)
ggplot(result_all_mean, aes(npuff, index_onset, color=npuff)) +
    ylim(c(-1,1)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(alpha=.3, outlier.shape=NA, aes(color=npuff, fill=npuff))  +
    scale_color_manual(values=c('0'='black','10'='red')) +
    scale_fill_manual(values=c('0'='black','10'='red')) +
    geom_line(aes(group=uniq),col='gray40', alpha=.7)

# stat
npuff.set = c(0, 10)
mytest.df.all = NULL
for(n in npuff.set){
    shapiro = subset(result_all_mean, npuff==n)$index_onset %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'result_all_mean_shapiro',npuff,'_stat.txt'), row.names=F, col.names=F)

    mytest = subset(result_all_mean, npuff==n)$index_onset %>% t.test
    mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t %>% as.data.frame
    colnames(mytest.df) = c('method','parameter','statistic','p.value')
    mytest.df$effsize = wilcoxonOneSampleRC(subset(result_all_mean, npuff==n)$index_onset, mu=0)
    mytest.df$npuff = n
    mytest.df.all = rbind(mytest.df.all, mytest.df)
}
mytest.df.all$p.adjusted = p.adjust(mytest.df.all$p.value, method='bonferroni')
write.csv(mytest.df.all, paste0(mydir, 'result_all_mean_stat.csv'), row.names=F)

### plot puffeffect
ggplot(puffeffect, aes(genotype, index_onset)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position="None") +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(outlier.shape=NA, alpha=.6, fill='black') +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)

# nsize
nsize = summary(as.factor(puffeffect$genotype)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0(mydir, 'nsize.csv'), row.names=F)

# stat
shapiro = puffeffect$index_onset %>% shapiro.test
print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'puffeffect_shapiro_stat.txt'), row.names=F, col.names=F)

mytest = puffeffect$index_onset %>% wilcox.test
mytest.df = with(mytest, c(method, statistic, p.value)) %>% as.data.frame %>% t %>% as.data.frame
colnames(mytest.df) = c('method','statistic','p.value')
mytest.df$effsize = wilcoxonOneSampleRC(puffeffect$index_onset, mu=0)
write.csv(mytest.df, paste0(mydir, 'puffeffect_stat.csv'), row.names=F)


# plot ethogram
pose_raw$label[pose_raw$label%in%c('','None')] = NA
pose_raw = na.omit(pose_raw)
pose_raw$trialtype = 'normal'
pose_raw$trialtype[grep('CALIB', pose_raw$filename)] = 'CALIB'
pose_raw$uniq = with(pose_raw, paste(genotype, ID, date, sep='_'))
pose_raw$uniq_trial_trialtype = with(pose_raw, paste(uniq, trial, trialtype, sep='_'))
for(k in c(0,10)){
    ethogram(subset(pose_raw, uniq_trial_trialtype%in%OKtrials4pose trialtype=='normal' & npuff==k))
}

### plot freq per pose per npuff per fly
pose_flymean.sub = subset(pose_flymean, trialtype=='normal' & label%in%c('grooming','stop','walk','PER'))
pose_flymean.sub$date = str_split_fixed(pose_flymean.sub$uniq, '_', 3)[,3]
ggplot(pose_flymean.sub, aes(npuff%>%as.factor,ratio, color=as.factor(npuff), fill=as.factor(npuff))) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_wrap(.~label, nrow=1) +
    geom_jitter(height=0, width=.2, size=.5, alpha=.6) +
    scale_color_manual(values=c('0'='black','10'='red')) +
    scale_fill_manual(values=c('0'='black','10'='red')) +
    geom_boxplot(alpha=.3, outlier.shape=NA, )


### plot puffeffect on pose_flymean
pose_puffeffect = subset(pose_puffeffect, trialtype=='normal' & label%in%c('grooming','stop','walk','PER'))
pose_puffeffect$date = with(pose_puffeffect, str_split_fixed(uniq, '_', 3)[,3])
ggplot(pose_puffeffect, aes(genotype, ratio)) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_grid(.~label) +
    geom_hline(yintercept=0) +
    geom_boxplot(outlier.shape=NA, alpha=.3, fill='black') +
    geom_jitter(height=0, width=.1, size=1, alpha=.6)

out = statPosePuffEffect.inGeno(pose_puffeffect, p.adjust.method='fdr')
print(out)
write.csv(out, paste0(mydir, 'pose_puffeffect_stat.csv'), row.names=F)
