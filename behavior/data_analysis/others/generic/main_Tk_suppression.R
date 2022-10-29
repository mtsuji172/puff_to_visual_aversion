source('init.R')
source('calc_indices.R')
source('./pose_estimate.R')

no_execute = function(){

# if exist, import data
export('../data/Tk_suppression/export/')

# pose estimation
loadData('../data/Tk_suppression/predictions_beforeIDsort/', '../data/Tk_suppression/statistics_beforeIDsort/')

# calc indices for all files
main_batch('../data/Tk_suppression/ballrecordings_beforeIDsort/', parallel=T)

# plot object position along time
mydf_all$genotype = factor(mydf_all$genotype, levels=c('UASTNT.','UASTNT.Tk1G4','UASTNT.Tk2G4','UASTNT.Tk3G4'))
mysubset = subset(mydf_all, trialtype=='normal' & uniq%in%c('UASTNT.Tk3G4_8_2020.11.24','UASTNT.Tk2G4_2_2020.12.08','UASTNT.Tk1G4_4_2020.11.22','UASTNT._7_2020.11.23'))
ggplot(subset(mysubset, t<=5), aes(t, abs(objectAngularPosition))) +
    facet_grid(uniq~.) +
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
result_all_mean$genotype = factor(result_all_mean$genotype, levels=c('UASTNT.','UASTNT.Tk1G4','UASTNT.Tk2G4','UASTNT.Tk3G4'))
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
puffeffect$genotype = factor(puffeffect$genotype, levels=c('UASTNT.','UASTNT.Tk1G4','UASTNT.Tk2G4','UASTNT.Tk3G4'))
mycol = c('UASTNT.'='black','UASTNT.Tk1G4'='blue','UASTNT.Tk2G4'='blue','UASTNT.Tk3G4'='blue')
mycol.outer = c('UASTNT.'='black','UASTNT.Tk1G4'='blue','UASTNT.Tk2G4'='deepskyblue3','UASTNT.Tk3G4'='blue')
myalpha = c('UASTNT.'=.3,'UASTNT.Tk1G4'=.3,'UASTNT.Tk2G4'=1,'UASTNT.Tk3G4'=.3)
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
result = NULL
for(x in 1:length(genotype.set)){
    out = t.test(subset(puffeffect, genotype == genotype.set[x])$index_onset)
    result = rbind(result,
                   data.frame(genotype=genotype.set[x],
                              p.value = out$p.value))
}
result$p.value = p.adjust(result$p.value, method='fdr')
print(result)
write.csv(result, './Tk_suppression/puffeffect_withinGeno_stat.csv', row.names=F)

# stat (between genotypes)
control = 'UASTNT.'
genotype.set = c('UASTNT.Tk1G4','UASTNT.Tk2G4','UASTNT.Tk3G4')
out = 
    lapply(1:length(genotype.set), function(x){
        result = 
            subset(puffeffect, genotype %in% c(control, genotype.set[x])) %>%
            {t.test(index_onset~genotype, .)}
        result$p.value
    }) %>% c
out = out %>% unlist %>% p.adjust(method='fdr')
names(out) = genotype.set
print(out)
write.csv(out, './Tk_suppression/puffeffect_interGeno_stat.csv', row.names=F)



### plot freq per pose per npuff per fly
pose_flymean.sub = subset(pose_flymean, trialtype=='normal' & label%in%c('grooming','stop','walk'))
pose_flymean.sub$genotype = factor(pose_flymean.sub$genotype, levels=c('UASTNT.','UASTNT.Tk1G4','UASTNT.Tk2G4','UASTNT.Tk3G4'))
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
pose_puffeffect.sub$genotype = factor(pose_puffeffect.sub$genotype, levels=c('UASTNT.','UASTNT.Tk1G4','UASTNT.Tk2G4','UASTNT.Tk3G4'))
mycol = c('UASTNT.'='black','UASTNT.Tk1G4'='blue','UASTNT.Tk2G4'='blue','UASTNT.Tk3G4'='blue')
mycol.outer = c('UASTNT.'='black','UASTNT.Tk1G4'='blue','UASTNT.Tk2G4'='deepskyblue3','UASTNT.Tk3G4'='blue')
myalpha = c('UASTNT.'=.3,'UASTNT.Tk1G4'=.3,'UASTNT.Tk2G4'=1,'UASTNT.Tk3G4'=.3)
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
print(out)
write.csv(out, './Tk_suppression/pose_puffeffect_stat_inGeno.csv', row.names=F)
out = statPosePuffEffect.interGeno(pose_puffeffect.sub, control='UASTNT.', p.adjust.method='fdr')
print(out)
write.csv(out, './Tk_suppression/pose_puffeffect_stat_interGeno.csv', row.names=F)

}
