source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

no_execute = function(){

# if exist, import data
export('../data/a3less/export/')

# pose estimation
loadData('../data/a3less/predictions/', '../data/a3less/statistics/')

# calc indices for all files
main_batch('../data/a3less/ballrecordings/', parallel=T)

# plot object position along time
mysubset = subset(mydf_all, trialtype=='normal' & uniq%in%c('CSsham_8_2020.08.12','CSa3less_0_2020.08.16'))
mysubset$genotype = factor(mysubset$genotype, levels=c('CSsham','CSa3less'))
ggplot(subset(mysubset, t<=5), aes(t, abs(objectAngularPosition))) +
    facet_grid(genotype~.) +
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


### plot aversion index
result_all_mean$genotype = factor(result_all_mean$genotype, levels=c('CSsham','CSa3less'))
result_all_mean$npuff = as.factor(result_all_mean$npuff)
ggplot(result_all_mean, aes(npuff, index_onset, color=npuff)) +
    facet_grid(.~genotype) +
    ylim(c(-1,1)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position='None') +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(alpha=.3, outlier.shape=NA, aes(color=npuff, fill=npuff))  +
    scale_color_manual(values=c('0'='black','10'='red')) +
    scale_fill_manual(values=c('0'='black','10'='red')) +
    geom_line(aes(group=uniq),col='gray40', alpha=.7)

### plot puffeffect
puffeffect$genotype = factor(puffeffect$genotype, levels=rev(unique(puffeffect$genotype)))
mycol = c('CSsham'='black','CSa3less'='blue')
ggplot(puffeffect, aes(genotype, index_onset, color=genotype, fill=genotype)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position="None") +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(outlier.shape=NA, alpha=.3) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6) +
    scale_color_manual(values=mycol) +
    scale_fill_manual(values=mycol)

# nsize
nsize = summary(as.factor(puffeffect$genotype)) %>% as.data.frame
print(nsize)
write.csv(nsize, './a3-/nsize.csv', row.names=F)


# stat (within genotypes)
genotype.set = unique(puffeffect$genotype)
pvalue = NULL
for(x in 1:length(genotype.set)){
    print(genotype.set[x])
    result = 
        subset(puffeffect, genotype == genotype.set[x])$index_onset %>%
        t.test
    pvalue = c(pvalue, result$p.value)
}
names(pvalue) = genotype.set
print(pvalue)
write.csv(pvalue, './a3-/puffeffect_stat_inGeno.csv', row.names=F)

# stat (between genotypes)
control = 'CSsham'
genotype.set = c('CSa3less')
out = 
    lapply(1:length(genotype.set), function(x){
        result = 
            subset(puffeffect, genotype%in%c(control, genotype.set[x])) %>%
            {t.test(index_onset~genotype, .)}
        result$p.value
    }) %>% c
names(out) = genotype.set
print(out)
write.csv(out, './a3-/puffeffect_stat_betweenGeno.csv', row.names=F)


}
