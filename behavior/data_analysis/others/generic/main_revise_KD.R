
source('init.R')
source('calc_indices.R')
source('pose_estimate.R')

mycolors = c('UASdicer2-.-UASTkRNAi.'='black','UASdicer2-.-Tk2G4.'='black','UASdicer2-.-UASTkRNAi.Tk2G4'='blue')

root = '/media/masato/free/file/behavior/data/'
mydir = 'revise/KD/'

# pose estimation
loadData(paste0(root,mydir,'/predictions/'), paste0(root,mydir,'/statistics/'))

# (optional) if manually-recorded "badtrials4aversion.csv" exists (flies stuck or flying)....
if(file.exists(paste0(root,mydir,'/badtrials4aversion.csv'))){
    badtrials.ori = read.csv(paste0(root,mydir,'/badtrials4aversion.csv'), header=F)[,1]
    myfiles = list.files(path=paste0(root,mydir,'/ballrecordings/'), pattern='.csv$')
    myfiles = myfiles[grep('GENOTYPE',myfiles)] %>% {gsub('.csv','',.)}
    myfiles = myfiles[myfiles %ni% badtrials.ori]

    OKtrials4aversion = 
        sapply(myfiles, function(myfile){
            myfile.sep = myfile %>% strsplit('_') %>% unlist
            myfile.out = paste(
                             myfile.sep[2] %>% {gsub('GENOTYPE','',.)},
                             myfile.sep[3] %>% {gsub('ID','',.)},
                             myfile.sep[1],
                             myfile.sep[5] %>% {gsub('TRIAL','',.)},
                             'normal',
                             sep = '_')
            myfile.out
        }) %>% unname
}

# calc indices for all files
main_batch(paste0(root,mydir,'ballrecordings/'), parallel=T)

# export data
dir.create(paste0(root,mydir,'/export/'), showWarnings=F)
export(paste0(root,mydir,'/export/'))

# import data
import(paste0(root,mydir,'/export/'))


### plot puffeffect
ggplot(puffeffect, aes(genotype %>% {gsub('UASdicer2-.-','',.)}, index_onset, fill=genotype, color=genotype)) +
	labs(x='',y='') +
    theme_classic() +
    theme(text=element_text(size=25), legend.position="None") +
    scale_color_manual(values=mycolors) +
    scale_fill_manual(values=mycolors) +
    geom_hline(yintercept=0, lty=2) +
    geom_boxplot(outlier.shape=NA, alpha=.6) +
    geom_jitter(height=0, width=.1, size=.8, alpha=.6)
ggsave(paste0(mydir, 'puffeffect.png'), height=5, width=3, dpi=300)

write.csv(select(puffeffect, c('uniq','genotype')), paste0(mydir, 'KD.csv'), row.names=F)
write.csv(select(puffeffect, c('genotype','index_onset')), paste0(mydir, 'raw.csv'), row.names=F)

# nsize
nsize = summary(as.factor(puffeffect$genotype)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0(root, mydir, 'export/nsize.csv'), row.names=T)


# stat (within genotype)
genotype.set = unique(puffeffect$genotype)

out = NULL
for(g in 1:length(genotype.set)){
    puffeffect.g = subset(puffeffect, genotype==genotype.set[g])
    shapiro = puffeffect.g$index_onset %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(root, mydir, 'export/puffeffect_shapiro_stat_',genotype.set[g],'.txt'), row.names=F, col.names=F)

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
write.csv(out, paste0(root, mydir, 'export/puffeffect_stat_ingeno.csv'), row.names=F)


# stat (between genotypes)
control = 'UASdicer2-.-UASTkRNAi.Tk2G4'
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
out$p.adjusted = p.adjust(out$p.value, method='bonferroni')
out
write.csv(out, paste0(root, mydir, 'export/puffeffect_stat_betweenGeno.csv'), row.names=F)

