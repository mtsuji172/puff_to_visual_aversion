source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

# if exist, import data
import('../data/Tk_mutants/export/')

# pose estimation
loadData('../data/Tk_mutants/predictions/', '../data/Tk_mutants/statistics/')

# calc indices for all files
main_batch('../data/Tk_mutants/ballrecordings/', parallel=T)

# export resuts
export('../data/Tk_mutants/export/')


# plot object position along time
mysubset = rbind(
                 subset(mydf_all, trialtype=='normal' & uniq=='CS_1_2020.04.15'),
                 subset(mydf_all, trialtype=='normal' & uniq=='DTk1_1_2020.04.26'),
                 subset(mydf_all, trialtype=='normal' & uniq=='DTk1.DTk2_4_2020.04.22'),
                 subset(mydf_all, trialtype=='normal' & uniq=='DTk2_2_2020.04.02')
                 )
mysubset$genotype = factor(mysubset$genotype, levels=c('CS','DTk1','DTk2','DTk1.DTk2'))
ggplot(subset(mysubset, t<=5), aes(t-.1, abs(objectAngularPosition))) +
    facet_grid(genotype~.) +
    geom_rect(data=mydf, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=mycol), color='white', alpha=.2) +
    scale_fill_manual(values=c('attraction'='blue', 'aversion'='red')) +
    geom_hline(yintercept=60, col='cyan3') +
    scale_color_manual(values=c('black','red')) +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, alpha=.6, lwd=2, se=T, aes(color=as.factor(npuff))) +
    coord_flip() +
    scale_x_reverse() +
    theme_classic() +
    theme(text=element_text(size=20), legend.position='None') +
    labs(x='', y='')


### plot aversion index
result_all_mean = result_all_mean
result_all_mean$genotype = factor(result_all_mean$genotype, levels=c('CS','DTk1','DTk2','DTk1.DTk2'))
result_all_mean$npuff = as.factor(result_all_mean$npuff)
ggplot(result_all_mean, aes(npuff, index_onset, color=npuff)) +
    facet_grid(.~genotype) +
    ylim(c(-1,1)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(alpha=.3, outlier.shape=NA, aes(color=npuff, fill=npuff))  +
    scale_color_manual(values=c('0'='black','1'='red','3'='red','5'='red','7'='red','10'='red')) +
    scale_fill_manual(values=c('0'='black','1'='red','3'='red','5'='red','7'='red','10'='red')) +
    geom_line(aes(group=uniq),col='gray40', alpha=.7)

### plot puffeffect
puffeffect$genotype = factor(puffeffect$genotype, levels=c('CS','DTk1','DTk2','DTk1.DTk2'))
mycol = c('CS'='black','DTk1'='blue','DTk2'='blue','DTk1.DTk2'='blue')
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
mystat = 
    lapply(1:length(genotype.set), function(x){
        result = 
            subset(puffeffect, genotype == genotype.set[x])$index_onset %>%
            t.test
        data.frame(genotype=genotype.set[x], p.value=result$p.value)
    }) %>% bind_rows
mystat$p.value = p.adjust(mystat$p.value, method='fdr')
print(mystat)
write.csv(mystat, './Tkmutants/puffeffect_ingeno_stat.csv', row.names=F)

# stat (between genotypes)
control = 'CS'
genotype.set = c('DTk1','DTk2','DTk1.DTk2')
mystat = 
    lapply(1:length(genotype.set), function(x){
        result = 
            subset(puffeffect, genotype %in% c(control, genotype.set[x])) %>%
            {t.test(index_onset~genotype, .)}
        result$p.value
    }) %>% c
mystat = mystat %>% unlist %>% p.adjust(method='fdr')
names(mystat) = genotype.set
print(mystat)
write.csv(mystat, './Tkmutants/puffeffect_betweengeno_stat.csv', row.names=F)



### plot freq per pose per npuff per fly
pose_flymean.sub = subset(pose_flymean, trialtype=='normal' & label%in%c('grooming','stop','walk'))
pose_flymean.sub$genotype = factor(pose_flymean.sub$genotype, levels=c('CS','DTk1','DTk2','DTk1.DTk2'))
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
pose_puffeffect.sub = subset(pose_puffeffect, trialtype=='normal' & label%in%c('grooming','stop','walk'))
pose_puffeffect.sub$genotype = factor(pose_puffeffect.sub$genotype, levels=c('CS','DTk1','DTk2','DTk1.DTk2'))
mycol = c('CS'='black','DTk1'='blue','DTk2'='blue','DTk1.DTk2'='blue')
pose_puffeffect.sub$date = with(pose_puffeffect.sub, str_split_fixed(uniq, '_', 3)[,3])
ggplot(pose_puffeffect.sub, aes(genotype, ratio, color=as.factor(genotype), fill=as.factor(genotype))) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_wrap(.~label) +
    geom_hline(yintercept=0) +
    geom_boxplot(outlier.shape=NA, alpha=.3) +
    geom_jitter(height=0, width=.1, size=1, alpha=.6) +
    scale_color_manual(values=mycol) +
    scale_fill_manual(values=mycol)

statPosePuffEffect.inGeno(pose_puffeffect.sub, p.adjust.method = 'fdr')
statPosePuffEffect.interGeno(pose_puffeffect.sub, control='CS', p.adjust.method = 'fdr')


}
