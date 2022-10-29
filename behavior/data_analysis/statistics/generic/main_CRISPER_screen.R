source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

# if exist, import data
import('../data/CRISPR_screen/export/')

# pose estimation
loadData('../data/CRISPR_screen/predictions/', '../data/CRISPR_screen/statistics/')

# calc indices for all files
main_batch('../data/CRISPR_screen/ballrecordings/', parallel=T)

uniq.myset = subset(result_all_mean, npuff==0 & index_onset<.4)$uniq %>%
    unique %>%
    {intersect(., OKflies)} %>%
    {intersect(.,unique(puffeffect$uniq))}

### plot aversion index
result_all_mean$genotype = as.factor(result_all_mean$genotype)
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
puffeffect$genotype = as.factor(puffeffect$genotype)
badflies1 = subset(puffeffect, genotype%ni%c('CCHa2SK2','CG13565_SK5','Dsk_SK1','npf_SK2','NPFR_SK8','Nplp1_SK1','RYa_SK4','SIFa_SK4','Tk_SK1') & index_onset < -.2)$uniq
badflies2 = subset(puffeffect, genotype%in%c('CCHa2SK2','CG13565_SK5','Dsk_SK1','npf_SK2','NPFR_SK8','Nplp1_SK1','RYa_SK4','SIFa_SK4','Tk_SK1') & index_onset < .4)$uniq
badflies = c(badflies1, badflies2)
ggplot(subset(puffeffect, uniq%ni%badflies), aes(genotype, index_onset)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position="None", axis.text.x = element_text(angle = 45, hjust=1)) +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(outlier.shape=NA, alpha=.3) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)



### plot freq per pose per npuff per fly
pose_flymean.sub = subset(pose_flymean, trialtype=='normal' & label%in%c('grooming','stop','walk') & uniq%in%uniq.myset & genotype!='TkCRISPR')
pose_flymean.sub$genotype = factor(pose_flymean.sub$genotype, levels=c('CS','DTk1','DTk2','DTk1.DTk2'))
ggplot(subset(pose_flymean.sub, uniq%in%uniq.myset), aes(npuff%>%as.factor,ratio, color=as.factor(npuff), fill=as.factor(npuff))) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_wrap(genotype~label, nrow=1) +
    geom_jitter(height=0, width=.2, size=.5, alpha=.6) +
    scale_color_manual(values=c('0'='black','10'='red')) +
    scale_fill_manual(values=c('0'='black','10'='red')) +
    geom_boxplot(alpha=.3, outlier.shape=NA, )


### plot puffeffect on pose_flymean
pose_puffeffect.sub = subset(pose_puffeffect, trialtype=='normal' & label%in%c('grooming','stop','walk') & uniq%in%uniq.myset & genotype!='TkCRISPR')
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

}
