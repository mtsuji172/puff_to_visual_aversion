source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

# import/export
import('../data/Tk_activation/export/')

# pose estimation
loadData('../data/Tk_activation/predictions/', '../data/Tk_activation/statistics/')

# calc indices for all files
main_batch('../data/Tk_activation/ballrecordings/', parallel=T)

# get N size
nsize = summary(as.factor(puffeffect$genotype)) %>% as.data.frame %>% t
print(nsize)
write.csv(nsize, './Tk_activation/nsize.csv', row.names=F)

# select flies for demo
mydf_all$genotype = factor(mydf_all$genotype, levels=(unique(mydf_all$genotype)))
mydf_all$LED = 'ON'
mydf_all$LED[mydf_all$LEDfreq==0] = 'OFF'
mydf_all.sub = subset(mydf_all, uniq%in%c('UASCsChrimson.-Tk2G4._4_2021.05.08','UASCsChrimson.-Tk2G4.-ATRnega_3_2021.05.30','UASCsChrimson.-DTk1.DTk2Tk2G4._12_2021.05.31') & LEDfreq%in%c(0,25))
ggplot(subset(mydf_all.sub, trialtype!='CALIB' & t<=5), aes(t, abs(objectAngularPosition))) +
    facet_grid(genotype~.) +
    theme_classic() +
    theme(text=element_text(size=20), legend.position='None') +
    coord_flip() +
    scale_x_reverse() +
    geom_rect(data=mydf, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=mycol), color='white', alpha=.2) +
    scale_fill_manual(values=c('attraction'='blue', 'aversion'='red')) +
    geom_hline(yintercept=60) +
    scale_color_manual(values=c('OFF'='black','ON'='red')) +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, alpha=.6, lwd=2, se=T, aes(color=as.factor(LED)))


# plot aversion index against LED freq
result_all_mean$genotype = factor(result_all_mean$genotype, levels=unique(result_all_mean$genotype))
result_all_mean$LEDfreq = as.factor(result_all_mean$LEDfreq)
ggplot(result_all_mean, aes(LEDfreq, index_onset)) +
	facet_wrap(.~genotype) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    ylim(c(-1,1)) +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(aes(group=LEDfreq), outlier.shape=NA) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)

# stat (each LEDfreq, per genotype)
genotype.set = unique(result_all_mean$genotype)
LEDfreq.set = unique(result_all_mean$LEDfreq)
out.all = lapply(1:length(genotype.set), function(g){
    print(genotype.set[g])
    subdata.g = subset(result_all_mean, genotype==genotype.set[g])
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



### plot puffeffect
mycol = c("UASCsChrimson.-Tk2G4.-ATRnega"='black', "UASCsChrimson.-Tk2G4."='blue', "UASCsChrimson.-DTk1.DTk2Tk2G4."='green4')
puffeffect$genotype = factor(puffeffect$genotype, levels=unique(puffeffect$genotype))
ggplot(subset(puffeffect, LEDfreq==25), aes(genotype, index_onset, color=genotype, fill=genotype)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    geom_hline(yintercept=0, lty=2) +
    scale_color_manual(values=mycol) +
    scale_fill_manual(values=mycol) +
    geom_boxplot(alpha=.3, outlier.shape=NA) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)


# stat (within genotype)
genotype.set = unique(puffeffect$genotype)
out.all = lapply(1:length(genotype.set), function(g){
    print(genotype.set[g])
    subdata.g = subset(puffeffect, LEDfreq==25 & genotype==genotype.set[g])
    out.g = t.test(subdata.g$index_onset)$p.value
    data.frame(genotype=genotype.set[g], LEDfreq=25, p.value=out.g)
}) %>% bind_rows
out.all$p.value = p.adjust(out.all$p.value, method='fdr')
print(out.all)
write.csv(out.all, './Tk_activation/puffeffect_withinGeno_stat.csv', row.names=F)

# stat (between genotypes)
control = 'UASCsChrimson.-Tk2G4.'
genotype.set = c('UASCsChrimson.-Tk2G4.-ATRnega','UASCsChrimson.-DTk1.DTk2Tk2G4.')
out = 
    lapply(1:length(genotype.set), function(g){
        puffeffect.sub = subset(puffeffect, genotype%in%c(control, genotype.set[g]))
        myp = t.test(index_onset~genotype, puffeffect.sub)$p.value
        result = data.frame(genotype=genotype.set[g], pvalue=myp)
    }) %>% bind_rows
out$pvalue = p.adjust(out$pvalue, method='fdr')
print(out)
write.csv(out.all, './Tk_activation/puffeffect_betweenGeno_stat.csv', row.names=F)


### plot freq per pose per LEDfreq per fly
pose_flymean.sub = subset(pose_flymean, trialtype=='normal' & label%in%c('glooming','stop','walk') & uniq%in%puffeffect$uniq)
pose_flymean.sub$genotype = pose_flymean.sub$genotype %>% as.character %>% {gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2','',.)}
pose_flymean.sub$genotype = factor(pose_flymean.sub$genotype, levels=unique(pose_flymean.sub$genotype))
pose_flymean.sub$LEDfreq = as.factor(pose_flymean.sub$LEDfreq)

# calc LED effect
uniq.myset = unique(pose_flymean$uniq)
label.set = c('glooming','stop','walk')
pose_flymean_diff = 
    lapply(1:length(uniq.myset), function(u){
       subdata.u = subset(pose_flymean.sub, uniq==uniq.myset[u])
        lapply(1:length(label.set), function(l){
           subdata.l = subset(subdata.u, label==label.set[l])
           out = subdata.l[1,]
           baseline = subset(subdata.l, LEDfreq==0)$ratio
           if((length(baseline)>0) & sum(subdata.l$LEDfreq==25)>0){
               out$ratio = subset(subdata.l, LEDfreq==25)$ratio - baseline
               out$LEDfreq = 25
               out
           }
         }) %>% bind_rows
     }) %>% bind_rows
pose_flymean_diff$LEDfreq = as.factor(pose_flymean_diff$LEDfreq)
pose_flymean_diff$genotype = factor(pose_flymean_diff$genotype, levels=rev(unique(pose_flymean_diff$genotype)))
pose_flymean_diff$genotype = factor(pose_flymean_diff$genotype, levels=c('UASCsChrimson.-Tk2G4.-ATRnega','UASCsChrimson.-Tk2G4.','UASCsChrimson.-DTk1.DTk2Tk2G4.'))
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
write.csv(pose_flymean_diff, '../data/Tk_activation/export/pose_puffeffect.csv', row.names=F)


# stat (per genotype per label)
label.set = c('glooming','stop','walk')
geno.set = unique(pose_flymean_diff$genotype)
stat.out =
    lapply(1:length(geno.set), function(g){
        subdata.g = subset(pose_flymean_diff, genotype==geno.set[g])
        out=  
            lapply(1:length(label.set), function(l){
                subdata.l = subset(subdata.g, label==label.set[l])
                out.l = subdata.l[1,c('genotype','LEDfreq','label')]
                out.l$p = t.test(subdata.l$ratio)$p.value
                out.l
            }) %>% bind_rows
        out
    }) %>% bind_rows
stat.out$p = p.adjust(stat.out$p, method='fdr')
print(stat.out)
write.csv(stat.out, './Tk_activation/pose_puffeffect_withinGeno_stat.csv', row.names=F)

# stat (per label , between genotype)
label.set = c('glooming','stop','walk')
ctrl = 'UASCsChrimson.-Tk2G4.'
geno.set = unique(pose_flymean_diff$genotype)
geno.set = geno.set[geno.set!=ctrl]
stat.out =
    lapply(1:length(label.set), function(l){
        subdata.l = subset(pose_flymean_diff, label==label.set[l])
        aov.out = aov(ratio ~ genotype, data=subdata.l)
        print(paste0("label: ", label.set[l]))
        print(summary(aov.out))
        out =  
            lapply(1:length(geno.set), function(g){
               subdata.g = subset(subdata.l, genotype%in%c(ctrl, geno.set[g]%>%as.character))
               out.g = subdata.g[1,c('genotype','label', 'LEDfreq')]
               out.g$p = t.test(ratio~genotype, data=subdata.g)$p.value
               out.g
             }) %>% bind_rows
        out
    }) %>% bind_rows
stat.out$p = p.adjust(stat.out$p, method='fdr')
print(stat.out)
write.csv(stat.out, './Tk_activation/pose_puffeffect_interGeno_stat.csv', row.names=F)

}
