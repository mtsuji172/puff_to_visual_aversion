source('init.R')
source('calc_indices.R')
source('./pose_estimate.R')

root = '/media/masato/free/file/behavior/data/'
mydir = 'Tk_suppression/'
dir.create(mydir, showWarnings=F)

# if exist, import data
import(paste0(root, mydir, 'export/'))

# pose estimation
loadData(paste0(rootpose, mydir, 'predictions/'), paste0(rootpose, mydir, 'statistics/'))

# calc indices for all files
main_batch(paste0(root, mydir, 'ballrecordings/'), parallel=T)

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
puffeffect = puffeffect %>% subset(uniq%in%uniq.myset & height==8)
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


# nsize
nsize = summary(as.factor(puffeffect$genotype)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0(mydir, 'nsize.csv'), row.names=F)


# stat (within genotype)
genotype.set = c('UASTNT.','UASTNT.Tk1G4', 'UASTNT.Tk2G4','UASTNT.Tk3G4')

out = NULL
for(g in 1:length(genotype.set)){
    puffeffect.g = subset(puffeffect, genotype==genotype.set[g])
    shapiro = puffeffect.g$index_onset %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'puffeffect_shapiro_stat_',genotype.set[g],'.txt'), row.names=F, col.names=F)

    if(shapiro$p.value < 0.05){
        mytest = wilcox.test(puffeffect.g$index_onset)
        effsize = wilcoxonOneSampleRC(puffeffect.g$index_onset, mu=0)
    } else{
        mytest = t.test(puffeffect.g$index_onset)
        effsize = with(puffeffect.g, mean(index_onset)/sd(index_onset))
    }
    mytest[sapply(mytest, is.null)] = NA
    mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
    colnames(mytest.df) = c('method','parameter','statistic','p.value')
    out.sub = data.frame(genotype=genotype.set[g], mytest.df, effsize)
    out = rbind(out, out.sub)
}
if(nrow(out) > 1){
    if(nrow(out) == 2)
        p.adjust.method = 'bonferroni'
    if(nrow(out) > 2)
        p.adjust.method = 'fdr'
    out$p.adjusted = p.adjust(out$p.value, method=p.adjust.method)
}
write.csv(out, paste0(mydir, 'puffeffect_stat_ingeno.csv'), row.names=F)


# stat (between genotypes)
control = 'UASTNT.'
genotype.set = genotype.set[genotype.set != control]
out = 
    lapply(1:length(genotype.set), function(g){
        puffeffect.g = subset(puffeffect, genotype%in%c(control, genotype.set[g]))
        puffeffect.g$genotype = factor(puffeffect.g$genotype, levels=c(genotype.set[g], control))
        shapiro_ctrl = shapiro.test(puffeffect.g$index_onset[puffeffect.g$genotype==control])$p.value
        shapiro_test = shapiro.test(puffeffect.g$index_onset[puffeffect.g$genotype==genotype.set[g]])$p.value

        if(shapiro_ctrl >= 0.05 & shapiro_test>= 0.05){
            mytest = t.test(index_onset ~ genotype, data = puffeffect.g)
            effsize = cohen.d(index_onset ~ genotype, data = puffeffect.g)$estimate
        } else{
            mytest = wilcox.test(index_onset ~ genotype, data = puffeffect.g)
            effsize = with(puffeffect.g, wilcoxonRG(index_onset, genotype))
        }
        mytest[sapply(mytest, is.null)] = NA
        mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
        colnames(mytest.df) = c('method','parameter','statistic','p.value')
        data.frame(genotype=genotype.set[g], mytest.df, effsize)
    }) %>% bind_rows
if(nrow(out) > 1){
    if(nrow(out) == 2)
        p.adjust.method = 'bonferroni'
    if(nrow(out) > 2)
        p.adjust.method = 'fdr'
    out$p.adjusted = p.adjust(out$p.value, method=p.adjust.method)
}
write.csv(out, paste0(mydir, 'puffeffect_stat_betweenGeno.csv'), row.names=F)



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
pose_puffeffect = subset(pose_puffeffect, trialtype=='normal' & label%in%c('grooming','stop','walk'))
pose_puffeffect$genotype = factor(pose_puffeffect$genotype, levels=c('UASTNT.','UASTNT.Tk1G4','UASTNT.Tk2G4','UASTNT.Tk3G4'))
mycol = c('UASTNT.'='black','UASTNT.Tk1G4'='blue','UASTNT.Tk2G4'='blue','UASTNT.Tk3G4'='blue')
mycol.outer = c('UASTNT.'='black','UASTNT.Tk1G4'='blue','UASTNT.Tk2G4'='deepskyblue3','UASTNT.Tk3G4'='blue')
myalpha = c('UASTNT.'=.3,'UASTNT.Tk1G4'=.3,'UASTNT.Tk2G4'=1,'UASTNT.Tk3G4'=.3)
pose_puffeffect$date = with(pose_puffeffect, str_split_fixed(uniq, '_', 3)[,3])
ggplot(pose_puffeffect, aes(genotype, ratio, color=as.factor(genotype), fill=as.factor(genotype))) + 
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

stat_pose.inGeno = statPosePuffEffect.inGeno(pose_puffeffect, p.adjust.method = 'fdr')
stat_pose.interGeno = statPosePuffEffect.interGeno(pose_puffeffect, control='UASTNT.', p.adjust.method = 'fdr')
write.csv(stat_pose.inGeno, paste0(mydir, 'stat_pose.inGeno.csv'), row.names=F)
write.csv(stat_pose.interGeno, paste0(mydir, 'stat_pose.interGeno.csv'), row.names=F)

