source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

# if exist, import data
import('../data/VGluTTk_suppression/export/')

# pose estimation
loadData('../data/VGluTTk_suppression/predictions/', '../data/VGluTTk_suppression/statistics/')

# calc indices for all files
activity_thresh = 0 # in this experiment, activity level was roughly half of the other experiments
main_batch('../data/VGluTTk_suppression/ballrecordings/', parallel=T)

# plot object position along time
mysubset = subset(mydf_all, trialtype=='normal' & uniq%in%c('UASFRTstopTNT.VGluTlexA-lexAopFLP.Tk2G4_6_2020.05.27','UASFRTstopTNT.-Tk2G4._2_2020.05.25','VGluTlexA.-lexAopFLP._1_2020.05.22'))
mysubset$genotype = factor(mysubset$genotype, levels=c('UASFRTstopTNT.-Tk2G4.','VGluTlexA.-lexAopFLP.','UASFRTstopTNT.VGluTlexA-lexAopFLP.Tk2G4'))
ggplot(subset(mysubset, t<=5), aes(t, abs(objectAngularPosition))) +
    facet_grid(genotype~.) +
    geom_rect(data=mydf, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=mycol), color='white', alpha=.2) +
    scale_fill_manual(values=c('attraction'='blue', 'aversion'='red')) +
    geom_hline(yintercept=60, col='cyan3') +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, alpha=.6, lwd=2, se=T, aes(color=as.factor(npuff))) +
    scale_color_manual(values=c('black','red')) +
    coord_flip() +
    scale_x_reverse() +
    theme_classic() +
    theme(text=element_text(size=20), legend.position='None') +
    labs(x='', y='')

### plot aversion index
result_all_mean$npuff = as.factor(result_all_mean$npuff)
ggplot(result_all_mean, aes(npuff, index_onset, color=npuff)) +
    facet_grid(.~genotype) +
    ylim(c(-1,1)) +
    labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(alpha=.3, outlier.shape=NA, aes(color=npuff, fill=npuff))  +
    scale_color_manual(values=c('0'='black','10'='red')) +
    scale_fill_manual(values=c('0'='black','10'='red')) +
    geom_line(aes(group=uniq),col='gray40', alpha=.7)

### plot puffeffect
puffeffect$genotype = factor(puffeffect$genotype, levels = c('UASFRTstopTNT.-Tk2G4.','VGluTlexA.-lexAopFLP.','UASFRTstopTNT.VGluTlexA-lexAopFLP.Tk2G4'))
mycol = c('UASFRTstopTNT.-Tk2G4.'='black','VGluTlexA.-lexAopFLP.'='black','UASFRTstopTNT.VGluTlexA-lexAopFLP.Tk2G4'='blue')
ggplot(puffeffect, aes(genotype, index_onset, color=genotype, fill=genotype)) +
    labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position="None") +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(outlier.shape=NA, alpha=.3) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6) +
    scale_color_manual(values=mycol) +
    scale_fill_manual(values=mycol)

# stat (within genotypes)
genotype.set = unique(puffeffect$genotype)
out = 
    lapply(1:length(genotype.set), function(x){
        result = 
            subset(puffeffect, genotype == genotype.set[x])$index_onset %>% t.test
        result$p.value
    }) %>% c
out = out %>% unlist %>% p.adjust(method='fdr')
names(out) = genotype.set
print(as.data.frame(out))
write.csv(as.data.frame(out), './VGluTTk_suppression/puffeffect_withinGeno_stat.csv', row.names=F)

# stat (between genotypes)
control = 'UASFRTstopTNT.VGluTlexA-lexAopFLP.Tk2G4'
genotype.set = c('UASFRTstopTNT.-Tk2G4.','VGluTlexA.-lexAopFLP.')
out = 
    lapply(1:length(genotype.set), function(x){
        result = 
            subset(puffeffect, genotype %in% c(control, genotype.set[x])) %>%
            {t.test(index_onset~genotype, .)}
        result$p.value
    }) %>% c
out = out %>% unlist %>% p.adjust(method='bonferroni')
names(out) = genotype.set
print(as.data.frame(out))
write.csv(as.data.frame(out), './VGluTTk_suppression/puffeffect_interGeno_stat.csv', row.names=F)

### plot freq per pose per npuff per fly
pose_flymean$genotype = factor(pose_flymean$genotype, levels = c('UASFRTstopTNT.-Tk2G4.','VGluTlexA.-lexAopFLP.','UASFRTstopTNT.VGluTlexA-lexAopFLP.Tk2G4'))
pose_flymean.sub = subset(pose_flymean, trialtype=='normal' & label%in%c('grooming','stop','walk'))
ggplot(pose_flymean.sub, aes(npuff%>%as.factor,ratio, color=as.factor(npuff), fill=as.factor(npuff))) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_wrap(genotype~label, nrow=1) +
    geom_jitter(height=0, width=.2, size=.5, alpha=.6) +
    scale_color_manual(values=c('0'='black','10'='red')) +
    scale_fill_manual(values=c('0'='black','10'='red')) +
    geom_boxplot(alpha=.3, outlier.shape=NA, )


### plot puffeffect on pose_flymean
pose_puffeffect$genotype = factor(pose_puffeffect$genotype, levels = c('UASFRTstopTNT.-Tk2G4.','VGluTlexA.-lexAopFLP.','UASFRTstopTNT.VGluTlexA-lexAopFLP.Tk2G4'))
pose_puffeffect.sub = subset(pose_puffeffect, trialtype=='normal' & label%in%c('grooming','stop','walk'))
mycol = c('UASFRTstopTNT.-Tk2G4.'='black','VGluTlexA.-lexAopFLP.'='black','UASFRTstopTNT.VGluTlexA-lexAopFLP.Tk2G4'='blue')
pose_puffeffect.sub$date = with(pose_puffeffect.sub, str_split_fixed(uniq, '_', 3)[,3])
ggplot(pose_puffeffect.sub, aes(genotype, ratio, color=as.factor(genotype), fill=as.factor(genotype))) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_wrap(.~label) +
    geom_hline(yintercept=0) +
    geom_boxplot(outlier.shape=NA, alpha=.5) +
    geom_jitter(height=0, width=.1, size=1, alpha=.6) +
    scale_color_manual(values=mycol) +
    scale_fill_manual(values=mycol)

# for Fig.S4c,d
statPosePuffEffect.inGeno(pose_puffeffect.sub, p.adjust.method='fdr')
statPosePuffEffect.interGeno(pose_puffeffect.sub, control='UASFRTstopTNT.VGluTlexA-lexAopFLP.Tk2G4', p.adjust.method='fdr')

}
