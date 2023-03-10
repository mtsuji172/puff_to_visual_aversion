library(dplyr)
library(ggplot2)
library(rcompanion)
library(effsize)

root = '/media/masato/forTransfer/data/main/revise/'
mydir = 'OMR/'
path2csv = paste0(root, mydir, 'ballrecordings/')
dir.create(mydir, showWarnings=F)

myfiles = paste0(path2csv, list.files(path = path2csv, pattern='*.csv'))
cl = makeCluster(numCores, type="FORK")
registerDoParallel(cl)
mydf_all =
    foreach(i=1:length(myfiles), .export=ls(.GlobalEnv)) %dopar% {
        # preprocess
        mydf = preprocess(myfiles[i]) %>% select(-c("x","y","postPuffInterval","host","height","width","AngularPosBias","npuff","acclimation","ID","filename","objectAngularPosition","objectAngularPosition_label","dObjectAngularPosition"))
        mydf$t = with(mydf, t / max(t) * 35)
        mydf$dObjectAngularPosition_label[mydf$dObjectAngularPosition_label=='zero'] = 0
        mydf$dObjectAngularPosition_label[mydf$dObjectAngularPosition_label=='R'] = 1
        mydf$dObjectAngularPosition_label[mydf$dObjectAngularPosition_label=='L'] = -1
        mydf$dObjectAngularPosition_label = as.numeric(mydf$dObjectAngularPosition_label)

        # standardize the schedule (so all trials would have been like so: still -> R -> still -> L -> still)
        initial_rotation = mydf$dObjectAngularPosition_label[which(mydf$dObjectAngularPosition_label != "zero") %>% min]
        if(initial_rotation == "L"){
            mydf$dObjectAngularPosition = - mydf$dObjectAngularPosition
            mydf$dx = - mydf$dx
            mydf$dObjectAngularPosition_label = - mydf$dObjectAngularPosition_label
        }

        # output
        mydf
    }
stopCluster(cl)
mydf_all = do.call("rbind", mydf_all)
write.csv(mydf_all, paste0(mydir, 'mydf_all.csv'), row.names=F)
mydf_all = read.csv(paste0(mydir, 'mydf_all.csv'))

ggplot(mydf_all %>% subset(sqrt(dx^2+dy^2)>10), aes(t, dx)) +
    theme_classic() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept= c(5,15,20,30)) +
    coord_flip() +
    scale_x_reverse() +
    geom_smooth()


# attraction index per trial, as well as activity index
result_all = 
    mydf_all %>% 
    mutate(uniq_trial = paste(uniq, trial, sep='_'), activity=sqrt(dx^2+dy^2)) %>%
    group_by(uniq_trial) %>%
    subset(activity > 15) %>%
    mutate(dx = dx * dObjectAngularPosition_label) %>%
    reframe(genotype=genotype[1], trial=trial[1], uniq=uniq[1], date=date[1], index=sum(dx)/sum(abs(dx)), activity = mean(activity))

# for each fly, pick best trial
result_all = 
    result_all %>%
        group_by(uniq) %>%
        mutate(index.max = max(index)) %>%
        subset(index == index.max)

# average of different trials of the same fly
result_all_mean = 
    result_all %>%
    group_by(uniq) %>%
    reframe(genotype=genotype[1], date=date[1], index = mean(index), activity= mean(activity))

ggplot(result_all_mean %>% subset(index>0), aes(genotype, index, color=genotype, fill=genotype)) +
    theme_classic() +
    theme(legend.position='None') +
    scale_y_continuous(expand = c(0, 0), limits = c(-1.1, 1.1)) +
    scale_color_manual(values=c('CS'='black', 'DTk1.DTk2'='blue')) +
    scale_fill_manual(values=c('CS'='black', 'DTk1.DTk2'='blue')) +
    geom_hline(yintercept=0, lty=2) +
    geom_point() +
    geom_boxplot(alpha=.3, outlier.shape=NA)
ggsave(paste0(root, mydir, 'index.png'), height=4, width=2.2, dpi=300)

# stat
write.csv(select(result_all_mean, c('genotype','index')), paste0('revise/',mydir, 'raw.csv'), row.names=F)
nsize = summary(as.factor(result_all_mean$genotype)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0('revise/', mydir, 'nsize.csv'), row.names=T)

genotype.set = unique(result_all_mean$genotype)
out = NULL
for(g in 1:length(genotype.set)){
    # get data
    result_all_mean.g = subset(result_all_mean, genotype==genotype.set[g])

    # shapiro
    shapiro = result_all_mean.g$index %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'shapiro_',genotype.set[g],'_stat.txt'), row.names=F, col.names=F)

    # t-test or Wilcoxon test
    if(shapiro$p.value < 0.05){
        mytest = wilcox.test(result_all_mean.g$index)
        effsize = wilcoxonOneSampleRC(result_all_mean.g$index, mu=0)
    } else{
        mytest = t.test(result_all_mean.g$index)
        effsize = with(result_all_mean.g, mean(index)/sd(index))
    }
    mytest[sapply(mytest, is.null)] = NA
    mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
    colnames(mytest.df) = c('method','parameter','statistic','p.value')
    out.sub = data.frame(genotype=genotype.set[g], mytest.df, effsize)
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
write.csv(out, paste0('revise/', mydir, 'stat_ingeno.csv'), row.names=F)
mystat = t.test(index ~ genotype, data=result_all_mean)
mystat %>% print %>% capture.output %>% write.table(paste0('revise/', mydir, 'stat.txt'), row.names=F, col.names=F)
