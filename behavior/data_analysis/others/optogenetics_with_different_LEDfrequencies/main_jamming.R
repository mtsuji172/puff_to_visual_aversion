source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

root = '/media/masato/free/file/behavior/data/'
mydir = 'VGluTTk_jamming/'
dir.create(mydir, showWarnings=F)

# if exist, import data
import(paste0(root, mydir, 'export/'))

# pose estimation
loadData(paste0(root, mydir, 'predictions/'), paste0(root, mydir, 'statistics/'))

# calc indices for all files
main_batch(paste0(root, mydir, 'ballrecordings/'), parallel=T)

# preprocessing
mydf_all$genotype = factor(mydf_all$genotype, levels=rev(unique(mydf_all$genotype)))
mydf_all$genotype = gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2G4','G4',mydf_all$genotype)
mydf_all$uniq = gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2G4','G4',mydf_all$uniq)
mydf_all$LED = 'ON'
mydf_all$LED[mydf_all$LEDfreq==0] = 'OFF'
uniq.myset = gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2G4','G4', uniq.myset)

# plot object position along time
ggplot(subset(mydf_all, uniq%in%c('G4_6_2021.04.16') & trialtype!='CALIB' & t<=5), aes(t, abs(objectAngularPosition))) +
    facet_grid(genotype~.) +
    theme_classic() +
    theme(text=element_text(size=20), legend.position='None') +
    coord_flip() +
    scale_x_reverse() +
    geom_rect(data=mydf, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=mycol), color='white', alpha=.2) +
    scale_fill_manual(values=c('attraction'='blue', 'aversion'='red')) +
    geom_hline(yintercept=60) +
    scale_color_manual(values=c('0'='black','1'='indianred1','3'='indianred1','6'='red','12'='indianred1','60'='indianred1')) +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, se=T, alpha=.6, aes(color=as.factor(LEDfreq)))


# plot aversion index against LED freq
result_all_mean$genotype = result_all_mean$genotype %>% as.character %>% {gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2','',.)}
result_all_mean = subset(result_all_mean, genotype=='G4')
result_all_mean$LEDfreq = as.factor(result_all_mean$LEDfreq)
result_all_mean$color.label[with(result_all_mean, LEDfreq==0)] = 'OFF'
result_all_mean$color.label[with(result_all_mean, LEDfreq==6)] = 'ON_main'
result_all_mean$color.label[with(result_all_mean, LEDfreq%ni%c(0,6))] = 'ON_other'
ggplot(result_all_mean, aes(LEDfreq, index_onset)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    ylim(c(-1,1)) +
    geom_hline(yintercept=0, lty=2) +
    scale_alpha_manual(values=c('OFF'=.3, 'ON_main'=1, 'ON_other'=.3)) +
    scale_color_manual(values=c('OFF'='black', 'ON_main'='indianred1', 'ON_other'='red')) +
    scale_fill_manual(values=c('OFF'='black', 'ON_main'='red', 'ON_other'='red')) +
    geom_boxplot(aes(group=LEDfreq, color=color.label, fill=color.label, alpha=color.label), outlier.shape=NA) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)

# stat (LEDfreq-wise)
LEDfreq.set = c(0, 1, 3, 6, 12, 60)

out = NULL
for(g in 1:length(LEDfreq.set)){
    result_all_mean.g = subset(result_all_mean, LEDfreq==LEDfreq.set[g])
    shapiro = result_all_mean.g$index_onset %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'result_all_mean_shapiro_stat_',LEDfreq.set[g],'.txt'), row.names=F, col.names=F)

    if(shapiro$p.value < 0.05){
        mytest = wilcox.test(result_all_mean.g$index_onset)
        effsize = wilcoxonOneSampleRC(result_all_mean.g$index_onset, mu=0)
    } else{
        mytest = t.test(result_all_mean.g$index_onset)
        effsize = with(result_all_mean.g, mean(index_onset)/sd(index_onset))
    }
    mytest[sapply(mytest, is.null)] = NA
    mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
    colnames(mytest.df) = c('method','parameter','statistic','p.value')
    out.sub = data.frame(LEDfreq=LEDfreq.set[g], mytest.df, effsize)
    out = rbind(out, out.sub)
}
if(nrow(out) > 1){
    if(nrow(out) == 2)
        p.adjust.method = 'bonferroni'
    if(nrow(out) > 2)
        p.adjust.method = 'fdr'
    out$p.adjusted = p.adjust(out$p.value, method=p.adjust.method)
}
write.csv(out, paste0(mydir, 'result_all_mean_stat_inLEDfreq.csv'), row.names=F)


### plot puffeffect
puffeffect$genotype = puffeffect$genotype %>% as.character %>% {gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2','',.)}
puffeffect = subset(puffeffect, genotype=='G4')
puffeffect$LEDfreq = as.factor(puffeffect$LEDfreq)
puffeffect$color.label[with(puffeffect, LEDfreq==6)] = 'main'
puffeffect$color.label[with(puffeffect, LEDfreq!=6)] = 'other'
ggplot(puffeffect, aes(LEDfreq, index_onset)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    geom_hline(yintercept=0, lty=2) +
    scale_alpha_manual(values=c('main'=1, 'other'=.3)) +
    scale_color_manual(values=c('main'='tomato', 'other'='red')) +
    scale_fill_manual(values=c('main'='red', 'other'='red')) +
    geom_boxplot(aes(group=LEDfreq, color=color.label, fill=color.label, alpha=color.label), outlier.shape=NA) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)

# nsize
nsize = summary(as.factor(puffeffect$LEDfreq)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0(mydir, 'nsize.csv'), row.names=F)

# stat (in LEDfreqs)
genotype.set = unique(puffeffect$genotype)
LEDfreq.set = LEDfreq.set[LEDfreq.set!=0]
out = NULL
for(g in 1:length(LEDfreq.set)){
    puffeffect.g = subset(puffeffect, LEDfreq==LEDfreq.set[g])
    shapiro = puffeffect.g$index_onset %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'puffeffect_shapiro_stat_',LEDfreq.set[g],'Hz.txt'), row.names=F, col.names=F)

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
    out.sub = data.frame(LEDfreq=LEDfreq.set[g], mytest.df, effsize)
    out = rbind(out, out.sub)
}
if(nrow(out) > 1){
    if(nrow(out) == 2)
        p.adjust.method = 'bonferroni'
    if(nrow(out) > 2)
        p.adjust.method = 'fdr'
    out$p.adjusted = p.adjust(out$p.value, method=p.adjust.method)
}
write.csv(out, paste0(mydir, 'puffeffect_stat_inLEDfreq.csv'), row.names=F)


# stat (between LEDfreqs)
summary(lm(index_onset ~ LEDfreq + uniq, data = puffeffect)) %>% capture.output %>% write.table(paste0(mydir, 'lm_puffeffect.txt'), row.names=F, col.names=F)

# plot ethogram
LEDfreq.set = unique(subset(pose_raw, trialtype=='normal')$LEDfreq) %>% sort
plots = lapply(1:length(LEDfreq.set), function(k){
                   myplot = ethogram(subset(pose_raw, trialtype=='normal' & LEDfreq==LEDfreq.set[k] & uniq_trial_trialtype_LEDfreq%in%OKtrials4pose_onset))
        }) %>% c
do.call('grid.arrange', c(plots, ncol=2, as.table=F))

### plot freq per pose per LEDfreq per fly
pose_flymean$genotype = pose_flymean$genotype %>% as.character %>% {gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2','',.)}
pose_flymean.sub = subset(pose_flymean, trialtype=='normal' & uniq%in%uniq.myset & label%in%c('glooming','stop','walk') & genotype=='UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2G4')
pose_flymean.sub$genotype = factor(pose_flymean.sub$genotype, levels=rev(unique(pose_flymean.sub$genotype)))
pose_flymean.sub$LEDfreq = as.factor(pose_flymean.sub$LEDfreq)
ggplot(pose_flymean.sub, aes(LEDfreq,ratio)) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_wrap(genotype~label, nrow=1) +
    geom_jitter(height=0, width=.2, size=.5, alpha=.6) +
    scale_color_manual(values=c('0'='black','1'='red','3'='red','6'='red','12'='red','60'='red')) +
    scale_fill_manual(values=c('0'='black','1'='red','3'='red','6'='red','12'='red','60'='red')) +
    geom_boxplot(aes(group=LEDfreq, color=LEDfreq, fill=LEDfreq), alpha=.3, outlier.shape=NA)

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
ggplot(pose_flymean.sub_mean, aes(LEDfreq,ratio)) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_wrap(genotype~label, nrow=1) +
    geom_jitter(height=0, width=.2, size=.5, alpha=.6) +
    scale_color_manual(values=c('OFF'='black','ON'='red')) +
    scale_fill_manual(values=c('OFF'='black','ON'='red')) +
    geom_boxplot(aes(group=LEDfreq, color=LEDfreq, fill=LEDfreq), alpha=.3, outlier.shape=NA)


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
# for pose_flymean.sub_mean...
pose_flymean_diff = 
    lapply(1:length(uniq.myset), function(u){
       subdata.u = subset(pose_flymean.sub_mean, uniq==uniq.myset[u])
        lapply(1:length(label.set), function(l){
           subdata.l = subset(subdata.u, label==label.set[l])
           baseline = subset(subdata.l, LEDfreq=='OFF')$ratio
           if(length(baseline)>0){
               subdata.l$ratio = subdata.l$ratio - baseline
               subdata.l
           }
         }) %>% bind_rows
     }) %>% bind_rows
pose_flymean_diff$LEDfreq = as.factor(pose_flymean_diff$LEDfreq)
pose_flymean_diff$genotype = factor(pose_flymean_diff$genotype, levels=rev(unique(pose_flymean_diff$genotype)))
ggplot(subset(pose_flymean_diff, LEDfreq!=0), aes(LEDfreq,ratio, color=genotype, fill=genotype)) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_wrap(genotype~label, nrow=1) +
    geom_hline(yintercept=0) +
    geom_jitter(height=0, width=.2, size=.5, alpha=.6) +
    scale_color_manual(values=mycol) +
    scale_fill_manual(values=mycol) +
    geom_boxplot(alpha=.3, outlier.shape=NA)

# calc fly-wise mean among LEDfreqs (0Hz vs other Hz)
label.set = unique(pose_flymean_diff$label)
pose_flymean_diff_mean =
    lapply(1:length(uniq.myset), function(k){
        lapply(1:length(label.set), function(l){
               subdata = subset(pose_flymean_diff, uniq==uniq.myset[k] & label==label.set[l])
               if(nrow(subdata)>0){
                   out = subdata[1,]
                   out$LEDfreq = 'ON'
                   out$ratio = mean(subdata$ratio)
                   out
               }
         }) %>% bind_rows
     }) %>% bind_rows
pose_flymean_diff_mean$genotype = factor(pose_flymean_diff_mean$genotype, levels=rev(unique(pose_flymean_diff_mean$genotype)))
ggplot(pose_flymean_diff_mean, aes(genotype, ratio, color=genotype, fill=genotype)) + 
    theme_classic() +
    theme(legend.position='None', text=element_text(size=20)) +
    labs(x='',y='') +
    facet_wrap(.~label, nrow=1) +
    geom_hline(yintercept=0) +
    geom_jitter(height=0, width=.2, size=.5, alpha=.6) +
    scale_color_manual(values=mycol) +
    scale_fill_manual(values=mycol) +
    geom_boxplot(alpha=.3, outlier.shape=NA)

# stat (per genotype per LEDfreq per label)
LEDfreq.set = unique(pose_flymean_diff$LEDfreq) 
LEDfreq.set = LEDfreq.set[LEDfreq.set%ni%c(0,'OFF')]
label.set = c('glooming','stop','walk')
geno.set = unique(pose_flymean_diff$genotype)
stat.out =
    lapply(1:length(geno.set), function(mygeno){
    subdata.g = subset(pose_flymean_diff, genotype==geno.set[mygeno])
    out=  
        lapply(1:length(label.set), function(l){
            subdata.l = subset(subdata.g, label==label.set[l])
            out.l = 
                lapply(1:length(LEDfreq.set), function(k){
                   subdata.k = subset(subdata.l, LEDfreq==LEDfreq.set[k])
                   out.k = subdata.k[1,c('genotype','LEDfreq','label')]
                   out.k$p = t.test(subdata.k$ratio)$p.value
                   out.k
                 }) %>% bind_rows
        }) %>% bind_rows
}) %>% bind_rows
stat.out$p = p.adjust(stat.out$p, method='fdr')
print(stat.out)

# stat (per label per LEDfreq, between genotype)
label.set = c('glooming','stop','walk')
ctrl = 'G4-ATRnega'
ctrl = 'UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2G4-ATRnega'
geno.set = unique(pose_flymean_diff$genotype)
geno.set = geno.set[geno.set!=ctrl]
freq.set = unique(pose_flymean_diff$LEDfreq)
freq.set = freq.set[freq.set!=0]
stat.out =
    lapply(1:length(label.set), function(l){
        subdata.l = subset(pose_flymean_diff, label==label.set[l])
        lapply(1:length(freq.set), function(f){
            subdata.f = subset(subdata.l, LEDfreq==freq.set[f])
            aov.out = aov(ratio ~ genotype, data=subdata.f)
            print(paste0("label: ", label.set[l], " freq: ", freq.set[f]))
            print(summary(aov.out))
            out =  
                lapply(1:length(geno.set), function(g){
                   subdata.g = subset(subdata.f, genotype%in%c(ctrl, geno.set[g]%>%as.character))
                   out.g = subdata.g[1,c('genotype','label', 'LEDfreq')]
                   out.g$p = t.test(ratio~genotype, data=subdata.g)$p.value
                   out.g
                 }) %>% bind_rows
        }) %>% bind_rows
    }) %>% bind_rows
stat.out$p = p.adjust(stat.out$p, method='fdr')
print(stat.out)



}
