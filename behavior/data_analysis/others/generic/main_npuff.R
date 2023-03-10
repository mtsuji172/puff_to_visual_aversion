source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

root = '/media/masato/free/file/behavior/data/'
mydir = 'CS_puffs/'
dir.create(mydir, showWarnings=F)

# if exist, import data
import(paste0(root, mydir, 'export/'))

# pose estimation
loadData(paste0(mydir,'/predictions/'), paste0(mydir,'/statistics/'))

# calc indices for all files
main_batch(paste0(mydir,'/ballrecordings/'), parallel=T)

# export data
export(paste0(mydir,'/export/'))

### plot activity index
result_all_mean$npuff = as.factor(result_all_mean$npuff)
baseline = mean(subset(result_all_mean, npuff==0)$activity)
result_all_mean$activity.rel = with(result_all_mean, activity/baseline)
ggplot(result_all_mean, aes(npuff, activity.rel, color=npuff, fill=npuff)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    scale_y_continuous(expand=c(0,0)) +
    expand_limits(y=0) +
    scale_color_manual(values=c('0'='black','1'='red','3'='red','5'='red','7'='red','10'='red')) +
    scale_fill_manual(values=c('0'='black','1'='red','3'='red','5'='red','7'='red','10'='red')) +
    stat_summary(fun=mean, geom='bar') +
    stat_summary(fun.data = mean_se, geom = "errorbar", width=.25, size=1) +
    geom_hline(yintercept=1, lty=2)

puffeffect$npuff =
    with(puffeffect, 
         factor(npuff, levels = npuff %>% as.character %>% as.numeric %>% unique %>% sort))
ggplot(puffeffect, aes(as.integer(npuff), activity)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position="None") +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(outlier.shape=NA, fill='red', color='gray40', aes(x=as.factor(npuff),alpha=npuff%>%as.character%>%as.numeric)) +
    scale_alpha_continuous(range=c(.3,.7)) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6) +
    geom_smooth(method='lm', lwd=2, se=F)

# stat (within npuff)
npuff.set = c(1, 3, 5, 7, 10)

out = NULL
for(g in 1:length(npuff.set)){
    puffeffect.g = subset(puffeffect, npuff==npuff.set[g])
    shapiro = puffeffect.g$activity %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'puffeffect_shapiro_stat_',npuff.set[g],'.txt'), row.names=F, col.names=F)

    if(shapiro$p.value < 0.05){
        mytest = wilcox.test(puffeffect.g$activity)
        effsize = wilcoxonOneSampleRC(puffeffect.g$activity, mu=0)
    } else{
        mytest = t.test(puffeffect.g$activity)
        effsize = with(puffeffect.g, mean(activity)/sd(activity))
    }
    mytest[sapply(mytest, is.null)] = NA
    mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
    colnames(mytest.df) = c('method','parameter','statistic','p.value')
    out.sub = data.frame(npuff=npuff.set[g], mytest.df, effsize)
    out = rbind(out, out.sub)
}
if(nrow(out) > 1){
    if(nrow(out) == 2)
        p.adjust.method = 'bonferroni'
    if(nrow(out) > 2)
        p.adjust.method = 'fdr'
    out$p.adjusted = p.adjust(out$p.value, method=p.adjust.method)
}
write.csv(out, paste0(mydir, 'puffeffect_activity_stat_inNpuff.csv'), row.names=F)

# stat (correlation among npuffs)
mycor = 
    with(puffeffect,
         cor.test(activity, npuff%>%as.integer))
print(mycor) %>% capture.output %>% write.table(paste0(mydir, 'puffeffect_activity_corstat_',mynpuff,'.txt'), row.names=F, col.names=F)



### plot puffeffect
puffeffect$npuff =
    with(puffeffect, 
         factor(npuff, levels = npuff %>% as.character %>% as.numeric %>% unique %>% sort))
ggplot(puffeffect, aes(as.integer(npuff), index_onset)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position="None") +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(outlier.shape=NA, fill='red', color='gray40', aes(x=as.factor(npuff),alpha=npuff%>%as.character%>%as.numeric)) +
    scale_alpha_continuous(range=c(.3,.7)) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6) +
    geom_smooth(method='lm', lwd=2, se=F)


# nsize
nsize = summary(as.factor(puffeffect$npuff)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0(mydir, 'nsize.csv'), row.names=F)


# stat (within npuff)
npuff.set = c(1, 3, 5, 7, 10)

out = NULL
for(g in 1:length(npuff.set)){
    puffeffect.g = subset(puffeffect, npuff==npuff.set[g])
    shapiro = puffeffect.g$index_onset %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'puffeffect_shapiro_stat_',npuff.set[g],'.txt'), row.names=F, col.names=F)

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
    out.sub = data.frame(npuff=npuff.set[g], mytest.df, effsize)
    out = rbind(out, out.sub)
}
if(nrow(out) > 1){
    if(nrow(out) == 2)
        p.adjust.method = 'bonferroni'
    if(nrow(out) > 2)
        p.adjust.method = 'fdr'
    out$p.adjusted = p.adjust(out$p.value, method=p.adjust.method)
}
write.csv(out, paste0(mydir, 'puffeffect_stat_inNpuff.csv'), row.names=F)

# stat (correlation among npuffs)
mycor = 
    with(puffeffect,
         cor.test(index_onset, npuff%>%as.integer))
print(mycor) %>% capture.output %>% write.table(paste0(mydir, 'puffeffect_corstat.txt'), row.names=F, col.names=F)


}
