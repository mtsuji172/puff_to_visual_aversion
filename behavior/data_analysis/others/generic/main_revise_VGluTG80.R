source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

mycolors = c('UASTNT.VGluTG80-Tk2G4.'='blue','VGluTG80.-Tk2G4.'='gray40','VGluTG80.-UASTNT.'='gray40','UASTNT.-Tk2G4.'='black')

mydir = 'revise/Tk2TNT-VGluTG80/'

# pose estimation
loadData(paste0('../data/main/',mydir,'/predictions/'), paste0('../data/main/',mydir,'/statistics/'))

# calc indices for all files
main_batch(paste0('../data/main/',mydir,'/ballrecordings/'), parallel=T)

# select flies with OK baseline visual response
uniq.myset =
    subset(result_all_mean, npuff==0)$uniq %>% 
    unique %>%
    {intersect(.,unique(puffeffect$uniq))}

# export data
dir.create(paste0('../data/main/',mydir,'/export/'), showWarnings=F)
export(paste0('../data/main/',mydir,'/export/'))

# import data
mydir = dirs[1]
import(paste0('../data/main/',mydir,'/export/'))

# plot object position along time
ggplot(subset(mydf_all, uniq%in%uniq.myset & uniq_trial_trialtype %in% OKtrials4aversion & height==8 & t<=5), aes(t-.1, abs(objectAngularPosition))) +
    facet_grid(host~genotype) +
    geom_rect(data=mydf, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=mycol), color='white', alpha=.2) +
    scale_x_continuous(expand = c(0, 0), trans='reverse') +
    scale_fill_manual(values=c('attraction'='blue', 'aversion'='red')) +
    geom_hline(yintercept=60, col='cyan3') +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, alpha=.6, lwd=2, se=T, aes(color=as.factor(npuff))) +
    scale_color_manual(values=c('black','red')) +
    coord_flip() +
    theme_classic() +
    theme(text=element_text(size=20), legend.position='None') +
    labs(x='', y='')
ggsave(paste0(mydir, '/mydf_all.png'), height=3, width=7, dpi=300)

# plot timecourse of activity
mydf_all$genotype = factor(mydf_all$genotype, levels=rev(unique(mydf_all$genotype)))
mydf_all$npuff = as.factor(mydf_all$npuff)
ggplot(subset(mydf_all, height==8 & uniq%in%uniq.myset & t>1 & t<59), aes(t, sqrt(dx^2+dy^2), color=npuff, fill=npuff)) +
    theme_classic() +
    scale_y_continuous(expand=c(0,0)) +
    expand_limits(y=0) +
    scale_color_manual(values=c('black','red')) +
    scale_fill_manual(values=c('black','red')) +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, se=T, size=1)
ggsave(paste0(mydir, '/activity_timecourse.png'), height=5, width=15, dpi=300)

###  aversion index
result_all_mean$npuff = as.factor(result_all_mean$npuff)
ggplot(subset(result_all_mean, uniq%in%uniq.myset), aes(npuff, index_onset, color=npuff)) +
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
ggsave(paste0(mydir, '/result_all_mean.png'), height=5, width=3, dpi=300)

### plot puffeffect
puffeffect$genotype = factor(puffeffect$genotype, levels=c('VGluTG80.-Tk2G4.','VGluTG80.-UASTNT.','UASTNT.-Tk2G4.','UASTNT.VGluTG80-Tk2G4.'))
ggplot(puffeffect, aes(genotype, index_onset, color=genotype, fill=genotype)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position="None") +
    geom_hline(yintercept=0, lty=2) +
    scale_color_manual(values=mycolors) +
    scale_fill_manual(values=mycolors) +
    geom_boxplot(outlier.shape=NA, alpha=.6) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)
ggsave(paste0(mydir, '/puffeffect.png'), height=5, width=3.5, dpi=300)

write.csv(select(puffeffect, c('genotype','index_onset')), paste0(mydir, 'raw.csv'), row.names=F)

# nsize
nsize = summary(as.factor(puffeffect$genotype)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0(mydir, 'nsize.csv'), row.names=T)


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
control = 'UASTNT.VGluTG80-Tk2G4.'
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
