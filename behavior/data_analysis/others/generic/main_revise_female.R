source('init.R')
source('calc_indices.R')
#source('pose_estimate.R')

mycolors = c('CS'='black','CSf'='blue')

mydir = 'revise/female/'

# pose estimation
loadData(paste0('../data/main/',mydir,'/predictions/'), paste0('../data/main/',mydir,'/statistics/'))


# calc indices for all files
main_batch(paste0('../data/main/',mydir,'/ballrecordings/'), parallel=T)

# export data
dir.create(paste0('../data/main/',mydir,'export/'), showWarnings=F)
export(paste0('../data/main/',mydir,'export/'))

# import data
mydir = dirs[1]
import(paste0('../data/main/',mydir,'export/'))


### plot puffeffect
ggplot(puffeffect, aes(genotype, index_onset, color=genotype, fill=genotype)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position="None", axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    scale_color_manual(values=mycolors) +
    scale_fill_manual(values=mycolors) +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(outlier.shape=NA, alpha=.6) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)
ggsave(paste0(mydir, '/puffeffect.png'), height=5, width=3, dpi=300)
write.csv(select(puffeffect, c('genotype','index_onset')), paste0(mydir, 'raw.csv'), row.names=F)

# nsize
nsize = summary(as.factor(puffeffect$genotype)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0(mydir, 'nsize.csv'), row.names=F)


# stat (within genotype)
genotype.set = unique(puffeffect$genotype)
out = NULL
for(g in 1:length(genotype.set)){
    puffeffect.g = subset(puffeffect, genotype==genotype.set[g])
    shapiro = puffeffect.g$index_onset %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'puffeffect_shapiro_stat_',genotype.set[g],'.txt'), row.names=F, col.names=F)

    if(shapiro$p.value < 0.05){
        mytest = wilcox.test(puffeffect.g$index_onset)
    } else{
        mytest = t.test(puffeffect.g$index_onset)
    }
    mytest[sapply(mytest, is.null)] = NA
    mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
    colnames(mytest.df) = c('method','parameter','statistic','p.value')
    out.sub = data.frame(genotype=genotype.set[g], mytest.df)
    out = rbind(out, out.sub)
}
if(nrow(out) > 1){
    if(nrow(out) == 2)
        p.adjust.method = 'bonferroni'
    if(nrow(out) > 2)
        p.adjust.method = 'fdr'
    out$p.adjusted = p.adjust(out$p.value, method=p.adjust.method)
}
out
write.csv(out, paste0(mydir, 'puffeffect_stat_ingeno.csv'), row.names=F)

# stat (between genotypes)
control = 'CS'
genotype.set = genotype.set[genotype.set!=control]
out = 
    lapply(1:length(genotype.set), function(g){
        puffeffect.g = subset(puffeffect, genotype%in%c(control, genotype.set[g]))
        shapiro_ctrl = shapiro.test(puffeffect.g$index_onset[puffeffect.g$genotype==control])$p.value
        shapiro_test = shapiro.test(puffeffect.g$index_onset[puffeffect.g$genotype==genotype.set[g]])$p.value

        if(shapiro_ctrl >= 0.05 & shapiro_test>= 0.05){
            mytest = t.test(index_onset ~ genotype, data = puffeffect.g)
        } else{
            mytest = wilcox.test(index_onset ~ genotype, data = puffeffect.g)
        }
        mytest[sapply(mytest, is.null)] = NA
        mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
        colnames(mytest.df) = c('method','parameter','statistic','p.value')
        data.frame(genotype=genotype.set[g], mytest.df)
    }) %>% bind_rows
out
write.csv(out, paste0(mydir, 'puffeffect_stat_betweenGeno.csv'), row.names=F)

