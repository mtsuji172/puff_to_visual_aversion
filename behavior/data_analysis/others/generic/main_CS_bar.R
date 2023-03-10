source('init_bar.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

root = '/media/masato/free/file/behavior/data/'
mydir = 'CS_bar/'
dir.create(mydir, showWarnings=F)

# if exist, import data
import(paste0(root, mydir, 'export/'))

# pose estimation
loadData('../data/CS_bar/predictions/', '../data/CS_bar/statistics/')

# calc indices for all files
main_batch('../data/CS_bar/ballrecordings/', parallel=T)


# plot object position along time
ggplot(subset(mydf_all, uniq=='CS_7_2021.09.20' & trialtype=='normal' & t<=30), aes(t, abs(objectAngularPosition))) +
    geom_rect(data=mydf, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=mycol), color='white', alpha=.2) +
    scale_fill_manual(values=c('attraction'='blue', 'aversion'='red')) +
    geom_hline(yintercept=60, col='cyan3') +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, alpha=.6, lwd=2, se=T, aes(color=as.factor(npuff))) +
    scale_color_manual(values=c('black','red')) +
    coord_flip() +
    scale_x_reverse() +
    theme_classic() +
    theme(text=element_text(size=20), legend.position='None') +
    labs(x='', y='')

# plot timecourse of activity
mydf_all$genotype = factor(mydf_all$genotype, levels=rev(unique(mydf_all$genotype)))
mydf_all$npuff = as.factor(mydf_all$npuff)
ggplot(subset(mydf_all, uniq%in%uniq.myset), aes(t, sqrt(dx^2+dy^2), color=npuff, fill=npuff)) +
    theme_classic() +
    scale_y_continuous(expand=c(0,0)) +
    expand_limits(y=0) +
    scale_color_manual(values=c('black','red')) +
    scale_fill_manual(values=c('black','red')) +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, se=T, size=1)

### plot aversion index
result_all_mean$npuff = as.factor(result_all_mean$npuff)
ggplot(result_all_mean, aes(npuff, -index_throughout, color=npuff)) +
    ylim(c(-1,1)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(alpha=.3, outlier.shape=NA, aes(color=npuff, fill=npuff))  +
    scale_color_manual(values=c('0'='black','10'='red')) +
    scale_fill_manual(values=c('0'='black','10'='red')) +
    geom_line(aes(group=uniq),col='gray40', alpha=.7)

# nsize
nsize = summary(as.factor(puffeffect$genotype)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0(mydir, 'nsize.csv'), row.names=F)


# stat (within npuff)
npuff.set = c(0, 10)

out = NULL
for(g in 1:length(npuff.set)){
    result_all_mean.g = subset(result_all_mean, npuff==npuff.set[g])
    shapiro = -result_all_mean.g$index_throughout %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'result_all_mean_shapiro_stat_',npuff.set[g],'.txt'), row.names=F, col.names=F)

    if(shapiro$p.value < 0.05){
        mytest = wilcox.test(-result_all_mean.g$index_throughout)
        effsize = wilcoxonOneSampleRC(-result_all_mean.g$index_throughout, mu=0)
    } else{
        mytest = t.test(-result_all_mean.g$index_throughout)
        effsize = with(result_all_mean.g, mean(-index_throughout)/sd(-index_throughout))
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
write.csv(out, paste0(mydir, 'result_all_mean_stat_inNpuff.csv'), row.names=F)

# stat (between npuff)

shapiro = -puffeffect$index_throughout %>% shapiro.test
write.csv(shapiro$p.value, paste0(mydir, '/puffeffect_shapiro_stat.csv'), row.names=F)

if(shapiro$p.value < 0.05){
    mytest = wilcox.test(-puffeffect$index_throughout)
    effsize = wilcoxonOneSampleRC(-puffeffect$index_throughout, mu=0)
} else{
    mytest = t.test(puffeffect$index_throughout)
    effsize = with(puffeffect, mean(-index_throughout)/sd(-index_throughout))
}
mytest[sapply(mytest, is.null)] = NA
mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t %>% as.data.frame
colnames(mytest.df) = c('method','parameter','statistic','p.value')
mytest.df$effsize = effsize
write.csv(mytest.df, paste0(mydir, 'puffeffect_stat.csv'), row.names=F)

}
