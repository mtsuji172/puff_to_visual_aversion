source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

mydir = '../data/CS_puffs/'

# pose estimation
loadData(paste0(mydir,'/predictions/'), paste0(mydir,'/statistics/'))

# calc indices for all files
main_batch(paste0(mydir,'/ballrecordings/'), parallel=T)

# export data
export(paste0(mydir,'/export/'))

# import data
import(paste0(mydir,'/export/'))

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

# stat (each npuff)
out = 
    lapply(1:length(npuff.set), function(x){
        result = 
            t.test(subset(puffeffect, npuff == npuff.set[x])$index_onset)
        data.frame(npuff = npuff.set[x],
                   pvalue = result$p.value)
    }) %>% bind_rows
out$pvalue = p.adjust(out$pvalue, method='fdr')
print(out)
write.csv(out, './CS_puffs/puffeffect_activity_stat.csv', row.names=F)


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

# stat (each npuff)
npuff.set = unique(puffeffect$npuff)
out = 
    lapply(1:length(npuff.set), function(x){
        result = 
            t.test(subset(puffeffect, npuff == npuff.set[x])$index_onset)
        data.frame(npuff = npuff.set[x],
                   pvalue = result$p.value)
    }) %>% bind_rows
out$pvalue = p.adjust(out$pvalue, method='fdr')
print(out)
write.csv(out, './CS_puffs/puffeffect_stat.csv', row.names=F)

# stat (correlation among npuffs)
mycor = 
    with(puffeffect,
         cor.test(index_onset, npuff%>%as.integer))
out = with(mycor, c(estimate, p.value))
names(out) = c('cor','p.value')
write.csv(as.data.frame(out)%>%t, './CS_puffs/puffeffect_corstat.csv', row.names=F)


}
