source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

root = '/media/masato/free/file/behavior/data/'
mydir = 'CS_intervals/'
dir.create(mydir, showWarnings=F)

# if exist, import data
import(paste0(root, mydir, 'export/'))

# pose estimation
loadData('../data/CS_intervals/predictions/', '../data/CS_intervals/statistics/')

# calc indices for all files
main_batch('../data/CS_intervals/ballrecordings/', parallel=T)

# As for npuff=0, various intervals should be averaged -> uniq.myset changes -> re-calculated puffeffect after averaging result_all_mean
base = subset(result_all_mean, npuff==0)
uniq.set = unique(base$uniq)
base.ave =
    lapply(1:length(uniq.set), function(u){
               subdata = subset(base, uniq==uniq.set[u])
               out = subdata[1,]
               out$index_onset = mean(subdata$index_onset)
               out$index_final = mean(subdata$index_final)
               out$index_throughout = mean(subdata$index_throughout)
               out$activity = mean(subdata$activity)
               out$postPuffInterval = 'base0'
               out
        }) %>% bind_rows
result_all_mean = rbind(subset(result_all_mean, npuff!=0), base.ave)
puffeffect = calcPuffeffect(result_all_mean)


### plot puffeffect
puffeffect$postPuffInterval =
    with(puffeffect, 
         factor(postPuffInterval, levels =
              sort(unique(postPuffInterval %>% as.character %>% as.numeric))))
ggplot(puffeffect, aes(postPuffInterval, index_onset)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position="None") +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(outlier.shape=NA, fill='red', color='gray40', aes(alpha=postPuffInterval%>%as.character%>%as.numeric)) +
    scale_alpha_continuous(range=c(.7,.3)) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)


# nsize
nsize = summary(as.factor(puffeffect$postPuffInterval)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0(mydir, 'nsize.csv'), row.names=F)


# stat (within genotype)
postPuffInterval.set = c(0, 1, 6, 10)

out = NULL
for(g in 1:length(postPuffInterval.set)){
    puffeffect.g = subset(puffeffect, postPuffInterval==postPuffInterval.set[g])
    shapiro = puffeffect.g$index_onset %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(mydir, '/puffeffect_shapiro_stat_',postPuffInterval.set[g],'.txt'), row.names=F, col.names=F)

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
    out.sub = data.frame(postPuffInterval=postPuffInterval.set[g], mytest.df, effsize)
    out = rbind(out, out.sub)
}
if(nrow(out) > 1){
    if(nrow(out) == 2)
        p.adjust.method = 'bonferroni'
    if(nrow(out) > 2)
        p.adjust.method = 'fdr'
    out$p.adjusted = p.adjust(out$p.value, method=p.adjust.method)
}
write.csv(out, paste0(mydir, 'puffeffect_stat_inInterval.csv'), row.names=F)

}
