source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

# import/export
export('../data/VGluTTk_activation/export/')

# pose estimation
loadData('../data/VGluTTk_activation/predictions/', '../data/VGluTTk_activation/statistics/')

# calc indices for all files
main_batch('../data/VGluTTk_activation/ballrecordings/')

# pre-processing
mydf_all$genotype = gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2G4','G4',mydf_all$genotype)
mydf_all$genotype = factor(mydf_all$genotype, levels=c('G4-ATRnega','G4'))
mydf_all$uniq = gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2G4','G4',mydf_all$uniq)
mydf_all$LED = 'ON'
mydf_all$LED[mydf_all$LEDfreq==0] = 'OFF'

# plot a select fly 
ggplot(subset(mydf_all, trialtype!='CALIB' & t<=5 & LEDfreq%in%c(0,1) & uniq%in%c('G4-ATRnega_6_2021.03.15','G4_9_2021.02.19')), aes(t, abs(objectAngularPosition))) +
    facet_grid(genotype~.) +
    theme_classic() +
    theme(text=element_text(size=20), legend.position='None') +
    coord_flip() +
    scale_x_reverse() +
    geom_rect(data=mydf, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=mycol), color='white', alpha=.2) +
    scale_fill_manual(values=c('attraction'='blue', 'aversion'='red')) +
    geom_hline(yintercept=60) +
    scale_color_manual(values=c('0'='black','1'='red')) +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, alpha=.6, lwd=2, se=T, aes(color=as.factor(LEDfreq)))


# plot aversion index against LED freq
result_all_mean$genotype = result_all_mean$genotype %>% as.character %>% {gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk1','',.)}
result_all_mean$genotype = factor(result_all_mean$genotype, levels=rev(unique(result_all_mean$genotype)))
result_all_mean$LEDfreq = as.factor(result_all_mean$LEDfreq)
result_all_mean$color.label = 'ctrl_ON'
result_all_mean$color.label[with(result_all_mean, genotype!='G4' & LEDfreq==0)] = 'ctrl_OFF'
result_all_mean$color.label[with(result_all_mean, genotype=='G4' & LEDfreq==0)] = 'test_OFF'
result_all_mean$color.label[with(result_all_mean, genotype=='G4' & LEDfreq==6)] = 'test_ON_main'
result_all_mean$color.label[with(result_all_mean, genotype=='G4' & LEDfreq%ni%c(0,6))] = 'test_ON_other'
ggplot(result_all_mean, aes(LEDfreq, index_onset)) +
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


### plot puffeffect
mycol = c('G4-ATRnega'='black','G4'='blue')
puffeffect$genotype = puffeffect$genotype %>% as.character %>% {gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2','',.)}
puffeffect$uniq = gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2','',puffeffect$uniq)
puffeffect$genotype = factor(puffeffect$genotype, levels=rev(unique(puffeffect$genotype)))
puffeffect$LEDfreq = as.factor(puffeffect$LEDfreq)
ggplot(puffeffect, aes(LEDfreq, index_onset)) +
	facet_wrap(.~genotype) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(aes(color=genotype, fill=genotype), outlier.shape=NA, alpha=.3) +
    scale_color_manual(values=mycol) +
    scale_fill_manual(values=mycol) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)

# stat (effect of each LEDfreq, per genotype)
genotype.set = unique(puffeffect$genotype)
LEDfreq.set = unique(puffeffect$LEDfreq)
LEDfreq.set = unique(puffeffect$LEDfreq[puffeffect$LEDfreq!=0])
out.all = lapply(1:length(genotype.set), function(g){
    print(genotype.set[g])
    subdata.g = subset(puffeffect, genotype==genotype.set[g])
    out.g = 
        lapply(1:length(LEDfreq.set), function(l){
                subdata.l = subset(subdata.g, LEDfreq == LEDfreq.set[l])
                t.test(subdata.l$index_onset)$p.value
        }) %>% c
    out.g = unlist(out.g)
    data.frame(genotype=genotype.set[g], LEDfreq=LEDfreq.set, p.value=out.g)
}) %>% bind_rows
out.all$p.value = p.adjust(out.all$p.value, method='fdr')
print(out.all)

# stat (within genotypes among LEDfreq)
genotype.set = unique(puffeffect$genotype)
for(x in 1:length(genotype.set)){
    print(genotype.set[x])
    result.aov = 
        subset(puffeffect, genotype == genotype.set[x]) %>%
        {aov(index_onset~as.factor(LEDfreq), data=.)}
    print(result.aov%>%summary)
    result.aov.out = result.aov%>%summary%>%unlist
    if(result.aov.out["Pr(>F)1"] < 0.05){
        result.HSD = 
            result.aov %>% TukeyHSD
        print(result.HSD)
    }
}

# stat (between genotypes for each LEDfreq)
control = 'G4-ATRnega'
genotype.set = c('G4')
LEDfreq.set = unique(puffeffect$LEDfreq)
out = 
    lapply(1:length(genotype.set), function(g){
        lapply(1:length(LEDfreq.set), function(x){
            puffeffect.sub = subset(puffeffect, genotype%in%c(control, genotype.set[g]) & LEDfreq == LEDfreq.set[x])
            myp = t.test(index_onset~genotype, puffeffect.sub)$p.value
            result = data.frame(genotype=genotype.set[g], LEDfreq=LEDfreq.set[x], pvalue=myp)
        }) %>% bind_rows
    }) %>% bind_rows
out$pvalue = p.adjust(out$pvalue, method='fdr')
print(out)
write.csv(out.all, './VGluTTk_activation/puffeffect_interGeno_stat.csv', row.names=F)

# calc fly-wise mean among LEDfreqs
uniq.myset = unique(puffeffect$uniq)
puffeffect_mean =
    lapply(1:length(uniq.myset), function(k){
           subdata = subset(puffeffect, uniq==uniq.myset[k])
           if(nrow(subdata)>0){
               out = subdata[1,]
               out$index_onset = mean(subdata$index_onset)
               out$index_throughout = mean(subdata$index_throughout)
               out$index_final = mean(subdata$index_final)
               out$activity = mean(subdata$activity)
               out
           }
     }) %>% bind_rows
puffeffect_mean$genotype = factor(puffeffect_mean$genotype, levels=rev(unique(puffeffect_mean$genotype)))
ggplot(puffeffect_mean, aes(genotype, index_onset, color=genotype, fill=genotype)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    geom_hline(yintercept=0, lty=2) +
    scale_color_manual(values=mycol) +
    scale_fill_manual(values=mycol) +
    geom_boxplot(alpha=.3, outlier.shape=NA) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)
t.test(subset(puffeffect_mean, genotype=='G4-ATRnega')$index_onset)
t.test(subset(puffeffect_mean, genotype=='G4')$index_onset)
t.test(index_onset~genotype, puffeffect_mean)


### calc freq per pose per LEDfreq per fly
pose_flymean$genotype = pose_flymean$genotype %>% as.character %>% {gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2','',.)}
pose_flymean$uniq = pose_flymean$uniq %>% as.character %>% {gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2','',.)}
pose_flymean.sub = subset(pose_flymean, trialtype=='normal' & label%in%c('glooming','stop','walk') & uniq%in%puffeffect$uniq)
pose_flymean.sub$genotype = factor(pose_flymean.sub$genotype, levels=rev(unique(pose_flymean.sub$genotype)))
pose_flymean.sub$LEDfreq = as.factor(pose_flymean.sub$LEDfreq)

# calc fly-wise mean among LEDfreqs (0Hz vs other Hz)
label.set = unique(pose_flymean.sub$label)
pose_flymean.sub_mean =
    lapply(1:length(uniq.myset), function(k){
        lapply(1:length(label.set), function(l){
               subdata = subset(pose_flymean.sub, uniq==uniq.myset[k] & label==label.set[l])
               subdata.OFF = subset(subdata, LEDfreq==0)
               subdata.ON = subset(subdata, LEDfreq!=0)
               if(nrow(subdata.OFF)>0 & nrow(subdata.ON)>0){
                   subdata.OFF$LEDfreq = 'OFF'
                   out.ON = subdata.ON[1,]
                   out.ON$LEDfreq = 'ON'
                   out.ON$ratio = mean(subdata.ON$ratio)
                   rbind(subdata.OFF, out.ON)
               }
         }) %>% bind_rows
     }) %>% bind_rows
pose_flymean.sub_mean$genotype = factor(pose_flymean.sub_mean$genotype, levels=rev(unique(pose_flymean.sub_mean$genotype)))

# calc LED effect
label.set = c('glooming','stop','walk')
pose_flymean_diff = 
    lapply(1:length(uniq.myset), function(u){
       subdata.u = subset(pose_flymean.sub, uniq==uniq.myset[u])
        lapply(1:length(label.set), function(l){
           subdata.l = subset(subdata.u, label==label.set[l])
           baseline = subset(subdata.l, LEDfreq==0)$ratio
           if(length(baseline)>0){
               subdata.l$ratio = subdata.l$ratio - baseline
               subdata.l
           }
         }) %>% bind_rows
     }) %>% bind_rows
pose_flymean_diff$genotype = factor(pose_flymean_diff$genotype, levels=rev(unique(pose_flymean_diff$genotype)))
ggplot(pose_flymean_diff, aes(LEDfreq, ratio, color=genotype, fill=genotype)) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_wrap(genotype~label, nrow=1) +
    geom_hline(yintercept=0) +
    geom_jitter(height=0, width=.2, size=.5, alpha=.6) +
    scale_color_manual(values=mycol) +
    scale_fill_manual(values=mycol) +
    geom_boxplot(alpha=.3, outlier.shape=NA)
write.csv(pose_flymean_diff, '/media/masato/free/file/behavior/data/VGluTTk_activation/export/pose_flymean_diff_LEDfreqwise.csv', row.names=F)

pose_flymean_diff = 
    lapply(1:length(uniq.myset), function(u){
       subdata.u = subset(pose_flymean.sub_mean, uniq==uniq.myset[u])
        lapply(1:length(label.set), function(l){
           subdata.l = subset(subdata.u, label==label.set[l])
           out = subdata.l[1,]
           baseline = subset(subdata.l, LEDfreq=='OFF')$ratio
           if(length(baseline)>0){
               out$ratio = subset(subdata.l, LEDfreq=='ON')$ratio - baseline
               select(out, -'LEDfreq')
           }
         }) %>% bind_rows
     }) %>% bind_rows
pose_flymean_diff$genotype = factor(pose_flymean_diff$genotype, levels=rev(unique(pose_flymean_diff$genotype)))
ggplot(pose_flymean_diff, aes(genotype, ratio, color=genotype, fill=genotype)) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_grid(.~label) +
    geom_hline(yintercept=0) +
    geom_jitter(height=0, width=.2, size=.5, alpha=.6) +
    scale_color_manual(values=mycol) +
    scale_fill_manual(values=mycol) +
    geom_boxplot(alpha=.3, outlier.shape=NA)
write.csv(pose_flymean_diff, '/media/masato/free/file/behavior/data/VGluTTk_activation/export/pose_flymean_diff_LEDfreqmean.csv', row.names=F)

# stat (per genotype per label)
label.set = c('glooming','stop','walk')
geno.set = unique(pose_flymean_diff$genotype)
stat.out =
    lapply(1:length(geno.set), function(g){
        subdata.g = subset(pose_flymean_diff, genotype==geno.set[g])
        out=  
            lapply(1:length(label.set), function(l){
                subdata.l = subset(subdata.g, label==label.set[l])
                out.l = subdata.l[1,c('genotype','label')]
                out.l$p = t.test(subdata.l$ratio)$p.value
                out.l
            }) %>% bind_rows
    }) %>% bind_rows
stat.out$p = p.adjust(stat.out$p, method='fdr')
print(stat.out)
write.csv(stat.out, './VGluTTk_activation/pose_puffeffect_withinGeno_stat.csv')

# stat (per label, between genotype)
label.set = c('glooming','stop','walk')
ctrl = 'G4-ATRnega'
geno.set = unique(pose_flymean_diff$genotype)
geno.set = geno.set[geno.set!=ctrl]
freq.set = unique(pose_flymean_diff$LEDfreq)
freq.set = freq.set[freq.set!=0]
stat.out =
    lapply(1:length(label.set), function(l){
        subdata.l = subset(pose_flymean_diff, label==label.set[l])
        aov.out = aov(ratio ~ genotype, data=subdata.l)
        print(paste0("label: ", label.set[l]))
        print(summary(aov.out))
        out =  
            lapply(1:length(geno.set), function(g){
               subdata.g = subset(subdata.l, genotype%in%c(ctrl, geno.set[g]%>%as.character))
               out.g = subdata.g[1,c('genotype','label')]
               out.g$p = t.test(ratio~genotype, data=subdata.g)$p.value
               out.g
             }) %>% bind_rows
        out
    }) %>% bind_rows
stat.out$p = p.adjust(stat.out$p, method='fdr')
print(stat.out)


}
