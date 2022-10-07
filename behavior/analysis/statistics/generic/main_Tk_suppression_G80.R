source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

# if exist, import data
import('../data/Tk_suppression_G80/export/')

# pose estimation
loadData('../data/Tk_suppression_G80/predictions/', '../data/Tk_suppression_G80/statistics/')

# calc indices for all files
main_batch('../data/Tk_suppression_G80/ballrecordings/', parallel=T)

# plot object position along time
mysubset = subset(mydf_all, trialtype=='normal' & uniq%in%c('VGluTG80MI09356.-Tk2G4._1_2021.01.14', 'UASTNT.-Tk2G4._17_2021.05.18', 'UASTNT.-Tk2G4.Cha7.4G80_2_2021.05.17', 'UASTNT.VGluTG80-Tk2G4._10_2021.05.22', 'UASTNT.-Tk2G4.Gad12AG80_2_2021.04.26'))
mysubset$genotype = factor(mysubset$genotype, levels=c('VGluTG80MI09356.-Tk2G4.','UASTNT.-Tk2G4.','UASTNT.-Tk2G4.Cha7.4G80','UASTNT.VGluTG80-Tk2G4.','UASTNT.-Tk2G4.Gad12AG80'))
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
result_all_mean$genotype = factor(result_all_mean$genotype, levels=c('UASTNT.-Tk2G4.','UASTNT.-Tk2G4.Cha7.4G80','UASTNT.VGluTG80-Tk2G4.','UASTNT.-Tk2G4.Gad12AG80'))
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
puffeffect$genotype = factor(puffeffect$genotype, levels=c('UASTNT.-Tk2G4.','UASTNT.-Tk2G4.Cha7.4G80','UASTNT.VGluTG80-Tk2G4.','UASTNT.-Tk2G4.Gad12AG80'))
mycol = c('UASTNT.-Tk2G4.'='black','UASTNT.VGluTG80-Tk2G4.'='blue','UASTNT.-Tk2G4.Cha7.4G80'='blue','UASTNT.-Tk2G4.Gad12AG80'='blue','TkSK1'='blue')
mycol.outer = c('UASTNT.-Tk2G4.'='black','UASTNT.VGluTG80-Tk2G4.'='deepskyblue3','UASTNT.-Tk2G4.Cha7.4G80'='blue','UASTNT.-Tk2G4.Gad12AG80'='blue','TkSK1'='blue')
myalpha = c('UASTNT.-Tk2G4.'=.3,'UASTNT.VGluTG80-Tk2G4.'=1,'UASTNT.-Tk2G4.Cha7.4G80'=.3,'UASTNT.-Tk2G4.Gad12AG80'=.3,'TkSK1'=.3)

ggplot(puffeffect, aes(genotype, index_onset, color=genotype, fill=genotype)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position="None") +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(outlier.shape=NA, aes(alpha=genotype)) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6) +
    scale_color_manual(values=mycol.outer) +
    scale_fill_manual(values=mycol) +
    scale_alpha_manual(values=myalpha)

# stat (within genotypes)
genotype.set = unique(puffeffect$genotype)
out = NULL
for(x in 1:length(genotype.set)){
    result = 
        subset(puffeffect, genotype == genotype.set[x])$index_onset %>%
        t.test
    out = rbind(out,
                data.frame(genotype = genotype.set[x],
                           p.value = result$p.value))
}
print(out)
write.csv(out, './Tk_suppression_G80/puffeffect_withinGeno_stat.csv', row.names=F)

# stat (between genotypes)
control = 'UASTNT.-Tk2G4.'
genotype.set = c('UASTNT.-Tk2G4.Cha7.4G80','UASTNT.VGluTG80-Tk2G4.','UASTNT.-Tk2G4.Gad12AG80')
out = 
    lapply(1:length(genotype.set), function(x){
        result = 
            subset(puffeffect, genotype %in% c(control, genotype.set[x])) %>%
            {t.test(index_onset~genotype, .)}
        result$p.value
    }) %>% c
out = out %>% unlist %>% p.adjust(method='fdr') %>% as.data.frame
out$genotype = genotype.set
print(out)
write.csv(out, './Tk_suppression_G80/puffeffect_interGeno_stat.csv', row.names=F)


### plot freq per pose per npuff per fly
pose_flymean.sub = subset(pose_flymean, trialtype=='normal' & label%in%c('grooming','stop','walk'))
pose_flymean.sub$genotype = factor(pose_flymean.sub$genotype, levels=c('UASTNT.-Tk2G4.','UASTNT.VGluTG80-Tk2G4.','UASTNT.-Tk2G4.Cha7.4G80','UASTNT.-Tk2G4.Gad12AG80'))
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
pose_puffeffect.sub$genotype = factor(pose_puffeffect.sub$genotype, levels=c('UASTNT.-Tk2G4.','UASTNT.VGluTG80-Tk2G4.','UASTNT.-Tk2G4.Cha7.4G80','UASTNT.-Tk2G4.Gad12AG80'))
pose_puffeffect.sub$date = with(pose_puffeffect.sub, str_split_fixed(uniq, '_', 3)[,3])
ggplot(pose_puffeffect.sub, aes(genotype, ratio, color=as.factor(genotype), fill=as.factor(genotype))) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_wrap(.~label) +
    geom_hline(yintercept=0) +
    geom_boxplot(outlier.shape=NA, aes(alpha=genotype)) +
    geom_jitter(height=0, width=.1, size=1, alpha=.6) +
    scale_color_manual(values=mycol.outer) +
    scale_fill_manual(values=mycol) +
    scale_alpha_manual(values=myalpha)

out = statPosePuffEffect.inGeno(pose_puffeffect.sub, p.adjust.method='fdr')
write.csv(out, './Tk_suppression_G80/pose_puffeffect_inGeno_stat.csv', row.names=F)
out = statPosePuffEffect.interGeno(pose_puffeffect.sub, control='UASTNT.-Tk2G4.', p.adjust.method = 'fdr')
write.csv(out, './Tk_suppression_G80/pose_puffeffect_interGeno_stat.csv', row.names=F)

}
