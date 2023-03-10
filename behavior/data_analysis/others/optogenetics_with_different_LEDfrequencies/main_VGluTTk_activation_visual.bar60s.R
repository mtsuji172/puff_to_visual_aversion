source('init.R')
source('calc_indices.R')
source('pose_estimate.R')


no_execute = function(){

root = '/media/masato/free/file/behavior/data/'
mydir = 'VGluTTk_activation_visual.bar60s/'
dir.create(mydir, showWarnings=F)

# if exist, import data
import(paste0(root, mydir, 'export/'))

# pose estimation
loadData(paste0(root, mydir, 'predictions/'), paste0(root, mydir, 'statistics/'))

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

# nsize
genotype.set = unique(puffeffect$genotype)
nsize = 
    lapply(1:length(genotype.set), function(g){
        nsize.g = 
           sapply(1:length(LEDfreq.set), function(l){
              subset(puffeffect, genotype==genotype.set[g] & LEDfreq==LEDfreq.set[l]) %>% nrow
           })
        data.frame(genotype=genotype.set[g], nsize = max(nsize.g))
    }) %>% bind_rows
print(nsize)
write.csv(nsize, paste0(mydir, 'nsize.csv'), row.names=F)

# LED effect (each genotype, freq-wise)
genotype.set = c('G4-ATRnega','G4')
LEDfreq.set = c(6, 12, 60)
out = NULL
for(g in 1:length(genotype.set)){
    for(l in 1:length(LEDfreq.set)){
        puffeffect.sub = subset(puffeffect, genotype==genotype.set[g] & LEDfreq==LEDfreq.set[l])
        shapiro = -puffeffect.sub$index_throughout %>% shapiro.test
        print(shapiro) %>% capture.output %>% write.table(paste0(mydir, '/puffeffect_shapiro_stat_',genotype.set[g],'_',LEDfreq.set[l],'Hz.txt'), row.names=F, col.names=F)

        if(shapiro$p.value < 0.05){
            mytest = wilcox.test(-puffeffect.sub$index_throughout)
            effsize = wilcoxonOneSampleRC(-puffeffect.sub$index_throughout, mu=0)
        } else{
            mytest = t.test(-puffeffect.sub$index_throughout)
            effsize = with(puffeffect.sub, mean(-index_throughout)/sd(-index_throughout))
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

}
