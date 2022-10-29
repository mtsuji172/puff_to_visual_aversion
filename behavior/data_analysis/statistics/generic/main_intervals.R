source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

# if exist, import data
export('../data/CS_intervals/export/')

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

# stat (each interval)
postPuffInterval.set = unique(puffeffect$postPuffInterval)
out = 
    lapply(1:length(postPuffInterval.set), function(x){
        result = 
            t.test(subset(puffeffect, postPuffInterval==postPuffInterval.set[x])$index_onset)
        data.frame(interval = postPuffInterval.set[x],
                   pvalue = result$p.value)
    }) %>% bind_rows
out$pvalue = p.adjust(out$pvalue, method='fdr')
print(out)
write.csv(out, './CS_intervals/puffeffect_stat.csv', row.names=F)

# show N
nsize = summary(as.factor(puffeffect$postPuffInterval)) %>% as.data.frame %>% t
print(nsize)
write.csv(nsize, './CS_intervals/nsize.csv', row.names=F)

}
