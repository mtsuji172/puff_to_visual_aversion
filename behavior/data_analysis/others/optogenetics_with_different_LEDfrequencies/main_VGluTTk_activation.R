source('init.R')
source('calc_indices.R')
source('pose_estimate.R')
library('rstatix')

no_execute = function(){

root = '/media/masato/free/file/behavior/data/'
mydir = 'VGluTTk_activation/'
dir.create(mydir, showWarnings=F)

# if exist, import data
import(paste0(root, mydir, 'export/'))

# pose estimation
loadData(paste0(root, mydir, 'predictions/'), paste0(root, mydir, 'statistics/'))


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

# nsize
nsize = summary(as.factor(puffeffect_mean$genotype)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0(mydir, 'nsize.csv'), row.names=F)

# stat (within genotype, LEDfreq-wise)
genotype.set = c('G4-ATRnega','G4')
LEDfreq.set = unique(puffeffect$LEDfreq)
out = NULL
for(g in 1:length(genotype.set)){
    for(l in 1:length(LEDfreq.set)){
        puffeffect.sub = subset(puffeffect, genotype==genotype.set[g] & LEDfreq==LEDfreq.set[l])
        shapiro = puffeffect.sub$index_onset %>% shapiro.test
        print(shapiro) %>% capture.output %>% write.table(paste0(mydir, '/puffeffect_shapiro_stat_',genotype.set[g],'_',LEDfreq.set[l],'Hz.txt'), row.names=F, col.names=F)

        if(shapiro$p.value < 0.05){
            mytest = wilcox.test(puffeffect.sub$index_onset)
            effsize = wilcoxonOneSampleRC(puffeffect.g$index_onset, mu=0)
        } else{
            mytest = t.test(puffeffect.sub$index_onset)
            effsize = with(puffeffect.g, mean(index_onset)/sd(index_onset))
        }
        mytest[sapply(mytest, is.null)] = NA
        mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
        colnames(mytest.df) = c('method','parameter','statistic','p.value')
        out.sub = data.frame(genotype=genotype.set[g], LEDfreq = LEDfreq.set[l], mytest.df, effsize)
        out = rbind(out, out.sub)
    }
}
if(nrow(out) > 1){
    if(nrow(out) == 2)
        p.adjust.method = 'bonferroni'
    if(nrow(out) > 2)
        p.adjust.method = 'fdr'
    out$p.adjusted = p.adjust(out$p.value, method=p.adjust.method)
}
write.csv(out, paste0(mydir, 'puffeffect_stat_ingeno.csv'), row.names=F)

# stat (between genotypes, LEDfreq-wise)
control = 'G4-ATRnega'
genotype.set = genotype.set[genotype.set != control]
out = 
    lapply(1:length(genotype.set), function(g){
        lapply(1:length(LEDfreq.set), function(l){
            puffeffect.sub = subset(puffeffect, genotype%in%c(control, genotype.set[g]) & LEDfreq==LEDfreq.set[l])
            puffeffect.sub$genotype = factor(puffeffect.sub$genotype, levels=c(genotype.set[g], control))
            shapiro_ctrl = shapiro.test(puffeffect.sub$index_onset[puffeffect.sub$genotype==control])$p.value
            shapiro_test = shapiro.test(puffeffect.sub$index_onset[puffeffect.sub$genotype==genotype.set[g]])$p.value

            if(shapiro_ctrl >= 0.05 & shapiro_test>= 0.05){
                mytest = t.test(index_onset ~ genotype, data = puffeffect.sub)
                effsize = cohen.d(index_onset ~ genotype, data = puffeffect.sub)$estimate
            } else{
                mytest = wilcox.test(index_onset ~ genotype, data = puffeffect.sub)
                effsize = with(puffeffect.sub, wilcoxonRG(index_onset, genotype))
            }
            mytest[sapply(mytest, is.null)] = NA
            mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
            colnames(mytest.df) = c('method','parameter','statistic','p.value')
            data.frame(genotype=genotype.set[g], LEDfreq=LEDfreq.set[l], mytest.df, effsize)
        }) %>% bind_rows
    }) %>% bind_rows
if(nrow(out) > 1){
    if(nrow(out) == 2)
        p.adjust.method = 'bonferroni'
    if(nrow(out) > 2)
        p.adjust.method = 'fdr'
    out$p.adjusted = p.adjust(out$p.value, method=p.adjust.method)
}
write.csv(out, paste0(mydir, 'puffeffect_stat_betweenGeno.csv'), row.names=F)

# stat (within genotype, LEDfreq averaged)
genotype.set = c('G4-ATRnega','G4')
out = NULL
for(g in 1:length(genotype.set)){
    puffeffect_mean.g = subset(puffeffect_mean, genotype==genotype.set[g])
    shapiro = puffeffect_mean.g$index_onset %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'puffeffect_mean_shapiro_stat_',genotype.set[g],'.txt'), row.names=F, col.names=F)

    if(shapiro$p.value < 0.05){
        mytest = wilcox.test(puffeffect_mean.g$index_onset)
        effsize = wilcoxonOneSampleRC(puffeffect_mean$index_onset, mu=0)
    } else{
        mytest = t.test(puffeffect_mean.g$index_onset)
        effsize = with(puffeffect_mean.g, mean(index_onset)/sd(index_onset))
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
write.csv(out, paste0(mydir, 'puffeffect_mean_stat_ingeno.csv'), row.names=F)


# stat (between genotypes, LEDfreq averaged)
control = 'G4-ATRnega'
genotype.set = genotype.set[genotype.set != control]
out = 
    lapply(1:length(genotype.set), function(g){
        puffeffect_mean.g = subset(puffeffect_mean, genotype%in%c(control, genotype.set[g]))
        puffeffect_mean.g$genotype = factor(puffeffect_mean.g$genotype, levels=c(genotype.set[g], control))
        shapiro_ctrl = shapiro.test(puffeffect_mean.g$index_onset[puffeffect_mean.g$genotype==control])$p.value
        shapiro_test = shapiro.test(puffeffect_mean.g$index_onset[puffeffect_mean.g$genotype==genotype.set[g]])$p.value

        if(shapiro_ctrl >= 0.05 & shapiro_test>= 0.05){
            mytest = t.test(index_onset ~ genotype, data = puffeffect_mean.g)
            effsize = cohen.d(index_onset ~ genotype, data = puffeffect_mean.g)$estimate
        } else{
            mytest = wilcox.test(index_onset ~ genotype, data = puffeffect_mean.g)
            effsize = with(puffeffect_mean.g, wilcoxonRG(index_onset, genotype))
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
write.csv(out, paste0(mydir, 'puffeffect_mean_stat_betweenGeno.csv'), row.names=F)


### calc freq per pose per LEDfreq per fly
pose_flymean$genotype = pose_flymean$genotype %>% as.character %>% {gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2','',.)}
pose_flymean$uniq = pose_flymean$uniq %>% as.character %>% {gsub('UASstopCsChrimson-VGluTlexA.-lexAopFLP.Tk2','',.)}
pose_flymean = subset(pose_flymean, trialtype=='normal' & label%in%c('glooming','stop','walk') & uniq%in%puffeffect$uniq)
pose_flymean$genotype = factor(pose_flymean$genotype, levels=rev(unique(pose_flymean$genotype)))
pose_flymean$LEDfreq = as.factor(pose_flymean$LEDfreq)

# calc fly-wise mean among LEDfreqs (0Hz vs other Hz)
label.set = unique(pose_flymean$label)
pose_flymean_mean =
    lapply(1:length(uniq.myset), function(k){
        lapply(1:length(label.set), function(l){
               subdata = subset(pose_flymean, uniq==uniq.myset[k] & label==label.set[l])
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
pose_flymean_mean$genotype = factor(pose_flymean_mean$genotype, levels=rev(unique(pose_flymean_mean$genotype)))

# calc LED effect
label.set = c('glooming','stop','walk')
pose_flymean_diff = 
    lapply(1:length(uniq.myset), function(u){
       subdata.u = subset(pose_flymean, uniq==uniq.myset[u])
        lapply(1:length(label.set), function(l){
           subdata.l = subset(subdata.u, label==label.set[l])
           baseline = subset(subdata.l, LEDfreq==0)$ratio
           if(length(baseline)>0){
               subdata.l$ratio = subdata.l$ratio - baseline
               subdata.l
           }
         }) %>% bind_rows
     }) %>% bind_rows
pose_flymean_diff = subset(pose_flymean_diff, LEDfreq!=0)
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

pose_flymean_mean_diff = 
    lapply(1:length(uniq.myset), function(u){
       subdata.u = subset(pose_flymean_mean, uniq==uniq.myset[u])
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
pose_flymean_mean_diff$genotype = factor(pose_flymean_mean_diff$genotype, levels=rev(unique(pose_flymean_mean_diff$genotype)))
ggplot(pose_flymean_mean_diff, aes(genotype, ratio, color=genotype, fill=genotype)) + 
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

# anova per pose pere genotype
genotype.set = unique(pose_flymean_diff$genotype)
label.set = unique(pose_flymean_diff$label)
LEDfreq.set = unique(pose_flymean_diff$LEDfreq) %>% as.character %>% as.integer
for(g in 1:length(genotype.set)){
    for(l in 1:length(label.set)){
        # get data
        pose_flymean_diff.sub = subset(pose_flymean_diff, genotype==genotype.set[g] & label==label.set[l], select=c(uniq, LEDfreq, ratio))
        pose_flymean_diff.sub$LEDfreq = factor(pose_flymean_diff.sub$LEDfreq, levels=LEDfreq.set)
        pose_flymean_diff.sub$uniq = as.factor(pose_flymean_diff.sub$uniq)

        # test normality
        shapiro.all = NULL
        for(f in 1:length(LEDfreq.set)){
            pose_flymean_diff.f = subset(pose_flymean_diff.sub, LEDfreq==LEDfreq.set[f])
            shapiro = pose_flymean_diff.f$ratio %>% shapiro.test
            print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'pose_flymean_diff_shapiro_stat_',genotype.set[g],'_',label.set[l],'_',LEDfreq.set[f],'Hz.txt'), row.names=F, col.names=F)
            shapiro.all = c(shapiro.all, shapiro$p.value)
        }

        # Kruskal-Wallis (Friedman not available as some combinations of uniq:LEDfreq are missing)
        if(max(shapiro.all) < 0.05){
            aov(ratio ~ LEDfreq, data = pose_flymean_diff.sub) %>% 
            capture.output %>%
            write.table(paste0(mydir, 'aov_pose_',genotype.set[g],'_',label.set[l],'.txt'), row.names=F, col.names=F)
        } else{
            kruskal.test(ratio ~ LEDfreq, data = pose_flymean_diff.sub) %>% 
            capture.output %>%
            write.table(paste0(mydir, 'kruskal_pose_',genotype.set[g],'_',label.set[l],'.txt'), row.names=F, col.names=F)
        }
    }
}

pose_flymean_diff.tmp = pose_flymean_diff
pose_flymean_diff.tmp$genotype = with(pose_flymean_diff.tmp, paste(genotype, LEDfreq, sep='_'))
stat_pose.inGeno.freqwise = statPosePuffEffect.inGeno(pose_flymean_diff.tmp, p.adjust.method = 'fdr')
LEDfreq.set = unique(pose_flymean_diff$LEDfreq)
stat_pose.interGeno.freqwise = 
    lapply(1:length(LEDfreq.set), function(l){
          data.frame(statPosePuffEffect.interGeno(pose_flymean_diff %>% subset(LEDfreq==LEDfreq.set[l]), control='G4-ATRnega', p.adjust.method = 'fdr'), LEDfreq = LEDfreq.set[l])
     }) %>% bind_rows

write.csv(stat_pose.inGeno, paste0(mydir, 'stat_pose.inGeno.csv'), row.names=F)
write.csv(stat_pose.interGeno, paste0(mydir, 'stat_pose.interGeno.csv'), row.names=F)

lapply(1:length(genotype.set), function(g){
    lapply(1:length(label.set), function(l){
        pose_flymean_diff.sub = subset(pose_flymean_diff, genotype==genotype.set[g] & label==label.set[l])
        aov(ratio ~ LEDfreq, data = pose_flymean_diff.sub) %>% 
            summary %>%
            capture.output %>%
            write.table(paste0(mydir, 'aov_pose.inGeno_genotype',genotype.set[g],'_label',label.set[l],'.txt'), row.names=F, col.names=F)
     })
 })

stat_pose.inGeno = statPosePuffEffect.inGeno(pose_flymean_mean_diff, p.adjust.method = 'fdr')
stat_pose.interGeno = statPosePuffEffect.interGeno(pose_flymean_mean_diff, control='G4-ATRnega', p.adjust.method = 'fdr')
write.csv(stat_pose.inGeno, paste0(mydir, 'stat_pose_mean.inGeno.csv'), row.names=F)
write.csv(stat_pose.interGeno, paste0(mydir, 'stat_pose_mean.interGeno.csv'), row.names=F)

}
