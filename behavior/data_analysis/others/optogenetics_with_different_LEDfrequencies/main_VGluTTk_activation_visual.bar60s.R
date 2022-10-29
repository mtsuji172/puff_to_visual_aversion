source('init.R')
source('calc_indices.R')
source('pose_estimate.R')


no_execute = function(){

# import/export
export('../data/VGluTTk_activation_visual.bar60s/export/')

# pose estimation
loadData('../data/VGluTTk_activation_visual.bar60s/predictions/', '../data/VGluTTk_activation_visual.bar60s/statistics/')

# calc indices for all files
main_batch('../data/VGluTTk_activation_visual.bar60s/ballrecordings/')

# preprocessing
mydf_all$genotype = factor(mydf_all$genotype, levels=rev(unique(mydf_all$genotype)))
mydf_all$genotype = gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2G4','G4',mydf_all$genotype)
mydf_all$uniq = gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2G4','G4',mydf_all$uniq)
mydf_all$LED = 'ON'
mydf_all$LED[mydf_all$LEDfreq==0] = 'OFF'

# plot a select fly 
mydf_all.sub = 
    rbind(
          subset(mydf_all, uniq=='G4_8_2021.08.24' & LEDfreq==0),
          subset(mydf_all, uniq=='G4_8_2021.09.01' & LEDfreq==6),
          subset(mydf_all, uniq=='G4_8_2021.08.29' & LEDfreq==12),
          subset(mydf_all, uniq=='G4_2_2021.08.24' & LEDfreq==60)
          )
ggplot(subset(mydf_all.sub, trialtype!='CALIB' & t<=30), aes(t, abs(objectAngularPosition))) +
    facet_grid(LEDfreq~.) +
    theme_classic() +
    theme(text=element_text(size=20), legend.position='None') +
    coord_flip() +
    scale_x_reverse() +
    geom_rect(data=mydf, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=mycol), color='white', alpha=.2) +
    scale_fill_manual(values=c('attraction'='blue', 'aversion'='red')) +
    geom_hline(yintercept=60) +
    scale_color_manual(values=c('0'='black','1'='indianred1','3'='indianred1','6'='red','12'='indianred1','60'='indianred1')) +
    scale_alpha_manual(values=c('0'=.3, '6'=1, '12'=.3, '60'=.3)) +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, alpha=.6, lwd=2, se=T, aes(color=as.factor(LEDfreq)))

# plot aversion index against LED freq
result_all_mean$genotype = result_all_mean$genotype %>% as.character %>% {gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2','',.)}
result_all_mean$genotype = factor(result_all_mean$genotype, levels=rev(unique(result_all_mean$genotype)))
result_all_mean$LEDfreq = as.factor(result_all_mean$LEDfreq)
result_all_mean$color.label = 'ctrl_ON'
result_all_mean$color.label[with(result_all_mean, genotype!='G4' & LEDfreq==0)] = 'ctrl_OFF'
result_all_mean$color.label[with(result_all_mean, genotype=='G4' & LEDfreq==0)] = 'test_OFF'
result_all_mean$color.label[with(result_all_mean, genotype=='G4' & LEDfreq==6)] = 'test_ON_main'
result_all_mean$color.label[with(result_all_mean, genotype=='G4' & LEDfreq%ni%c(0,6))] = 'test_ON_other'
ggplot(result_all_mean, aes(LEDfreq, -index_throughout)) +
	facet_wrap(.~genotype) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    ylim(c(-1,1)) +
    geom_hline(yintercept=0, lty=2) +
    scale_alpha_manual(values=c('ctrl_OFF'=.3, 'ctrl_ON'=.3, 'test_OFF'=.3, 'test_ON_main'=1, 'test_ON_other'=.3)) +
    scale_color_manual(values=c('ctrl_OFF'='black', 'ctrl_ON'='red', 'test_OFF'='black', 'test_ON_main'='indianred1', 'test_ON_other'='red')) +
    scale_fill_manual(values=c('ctrl_OFF'='black', 'ctrl_ON'='red', 'test_OFF'='black', 'test_ON_main'='red', 'test_ON_other'='red')) +
    geom_boxplot(aes(group=LEDfreq, color=color.label, fill=color.label, alpha=color.label), outlier.shape=NA) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)

# stat (each LEDfreq, per genotype)
genotype.set = unique(result_all_mean$genotype)
genotype.set = c('G4', 'G4-ATRnega')
LEDfreq.set = unique(result_all_mean$LEDfreq)
out.all = lapply(1:length(genotype.set), function(g){
    print(genotype.set[g])
    subdata.g = subset(result_all_mean, uniq%in%uniq.myset & genotype==genotype.set[g])
    out.g = 
        lapply(1:length(LEDfreq.set), function(l){
                subdata.l = subset(subdata.g, LEDfreq == LEDfreq.set[l])
                t.test(subdata.l$index_throughout)$p.value
        }) %>% c
    out.g = unlist(out.g)
    data.frame(genotype=genotype.set[g], LEDfreq=LEDfreq.set, p.value=out.g)
}) %>% bind_rows
out.all$p.value = p.adjust(out.all$p.value, method='fdr')
print(out.all)


### plot puffeffect
mycol = c('G4-ATRnega'='black','G4'='blue')
puffeffect$genotype = puffeffect$genotype %>% as.character %>% {gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2','',.)}
puffeffect$genotype = factor(puffeffect$genotype, levels=rev(unique(puffeffect$genotype)))
puffeffect$LEDfreq = as.factor(puffeffect$LEDfreq)
puffeffect$color.label = 'ctrl'
puffeffect$color.label[with(puffeffect, genotype=='G4' & LEDfreq==6)] = 'main'
puffeffect$color.label[with(puffeffect, genotype=='G4' & LEDfreq!=6)] = 'other'
ggplot(puffeffect, aes(LEDfreq, -index_throughout)) +
	facet_wrap(.~genotype) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    geom_hline(yintercept=0, lty=2) +
    scale_alpha_manual(values=c('ctrl'=.3, 'main'=1, 'other'=.3)) +
    scale_color_manual(values=c('ctrl'='black', 'main'='tomato', 'other'='red')) +
    scale_fill_manual(values=c('ctrl'='black', 'main'='red', 'other'='red')) +
    geom_boxplot(aes(group=LEDfreq, color=color.label, fill=color.label, alpha=color.label), outlier.shape=NA) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)

# stat (effect of each LEDfreq, per genotype)
uniq.myset = puffeffect$uniq %>% unique
genotype.set = unique(puffeffect$genotype)
LEDfreq.set = unique(puffeffect$LEDfreq)
LEDfreq.set = unique(puffeffect$LEDfreq[puffeffect$LEDfreq!=0])
out.all = lapply(1:length(genotype.set), function(g){
    print(genotype.set[g])
    subdata.g = subset(puffeffect, genotype==genotype.set[g])
    out.g = 
        lapply(1:length(LEDfreq.set), function(l){
                subdata.l = subset(subdata.g, LEDfreq == LEDfreq.set[l])
                t.test(subdata.l$index_throughout)$p.value
        }) %>% c
    out.g = unlist(out.g)
    data.frame(genotype=genotype.set[g], LEDfreq=LEDfreq.set, p.value=out.g)
}) %>% bind_rows
out.all$p.value = p.adjust(out.all$p.value, method='fdr')
print(out.all)
write.csv(out.all, './VGluTTk_activation_visual.bar60s/puffeffect_stat.csv', row.names=F)

# stat (within genotypes among LEDfreq)
genotype.set = unique(puffeffect$genotype)
for(x in 1:length(genotype.set)){
    result = 
        subset(puffeffect, uniq%in%uniq.myset & genotype == genotype.set[x]) %>%
        {aov(index_throughout~as.factor(LEDfreq), data=.)} %>%
        TukeyHSD
    print(genotype.set[x])
    print(result)
}

# stat (between genotypes for each LEDfreq)
control = 'G4-ATRnega'
genotype.set = c('G4')
LEDfreq.set = unique(puffeffect$LEDfreq)
out = 
    lapply(1:length(genotype.set), function(g){
        lapply(1:length(LEDfreq.set), function(x){
            puffeffect.sub = subset(puffeffect, uniq%in%uniq.myset & genotype%in%c(control, genotype.set[g]) & LEDfreq == LEDfreq.set[x])
            myp = t.test(index_throughout~genotype, puffeffect.sub)$p.value
            result = data.frame(genotype=genotype.set[g], LEDfreq=LEDfreq.set[x], pvalue=myp)
        }) %>% bind_rows
    }) %>% bind_rows
out$pvalue = p.adjust(out$pvalue, method='fdr')
print(out)
write.csv(out.all, './VGluTTk_activation_visual.bar60s/puffeffect_interGeno_stat.csv', row.names=F)


}
