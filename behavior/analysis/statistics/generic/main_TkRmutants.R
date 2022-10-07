source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

# if exist, import data
import('../data/TkR_mutants/export/')

# pose estimation
loadData('../data/TkR_mutants/predictions/', '../data/TkR_mutants/statistics/')

# calc indices for all files
main_batch('../data/TkR_mutants/ballrecordings/', parallel=T)

export('../data/TkR_mutants/export/')

# plot object position along time
mysubset = rbind(
                 subset(mydf_all, trialtype=='normal' & uniq=='CS_0_2021.10.08'),
                 subset(mydf_all, trialtype=='normal' & uniq=='Takr86CF28_2_2020.09.29'),
                 subset(mydf_all, trialtype=='normal' & uniq=='Takr99DMB09356_8_2020.07.08')
                 )
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
result_all_mean$genotype = factor(result_all_mean$genotype, levels=c('CS','Takr86CF28','Takr99DMB09356'))
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
puffeffect$genotype = factor(puffeffect$genotype, levels=rev(unique(puffeffect$genotype)))
mycol = c('CS'='black','Takr86CF28'='blue','Takr99DMB09356'='blue')
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
            subset(puffeffect, genotype==genotype.set[x])$index_onset %>% t.test
        result$p.value
    }) %>% c
out = out %>% unlist %>% p.adjust(method='fdr')
names(out) = genotype.set
print(out)
write.csv(out, './TkRmutants/puffeffect_wihtinGeno_stat.csv', row.names=F)

# stat (between genotypes)
control = 'CS'
genotype.set = c('Takr86CF28','Takr99DMB09356')
out = 
    lapply(1:length(genotype.set), function(x){
        result = 
            subset(puffeffect, genotype %in% c(control, genotype.set[x])) %>%
            {t.test(index_onset~genotype, .)}
        result$p.value
    }) %>% c
out = out %>% unlist %>% p.adjust(method='bonferroni')
names(out) = genotype.set
print(out)
write.csv(out, './TkRmutants/puffeffect_interGeno_stat.csv', row.names=F)


# plot ethogram
pose_raw$label[pose_raw$label%in%c('','None')] = NA
pose_raw = na.omit(pose_raw)
pose_raw$trialtype = 'normal'
pose_raw$trialtype[grep('CALIB', pose_raw$filename)] = 'CALIB'
pose_raw$uniq = with(pose_raw, paste(genotype, ID, date, sep='_'))
pose_raw$uniq_trial_trialtype = with(pose_raw, paste(uniq, trial, trialtype, sep='_'))

### plot freq per pose per npuff per fly
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
pose_puffeffect = subset(pose_puffeffect, trialtype=='normal' & label%in%c('grooming','stop','walk'))
mycol = c('CS'='black','Takr86CF28'='blue','Takr99DMB09356'='blue')
pose_puffeffect$date = with(pose_puffeffect, str_split_fixed(uniq, '_', 3)[,3])
ggplot(pose_puffeffect, aes(genotype, ratio, color=as.factor(genotype), fill=as.factor(genotype))) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_wrap(.~label) +
    geom_hline(yintercept=0) +
    geom_boxplot(outlier.shape=NA, alpha=.3) +
    geom_jitter(height=0, width=.1, size=1, alpha=.6) +
    scale_color_manual(values=mycol) +
    scale_fill_manual(values=mycol)

statPosePuffEffect.inGeno(pose_puffeffect, p.adjust.method = 'fdr')
statPosePuffEffect.interGeno(pose_puffeffect, control='CS', p.adjust.method = 'fdr')

}
