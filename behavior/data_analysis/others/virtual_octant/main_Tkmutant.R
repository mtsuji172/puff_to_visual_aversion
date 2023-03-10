library(ggplot2)
library(dplyr)
library(rcompanion)
library(effsize)

#######################
# params
#######################
root = '/media/masato/free/file/behavior/data/'
mydir = 'Tkmutant_virtualoctant_puff/'
dir.create(mydir, showWarnings=F)

# read data
mydata = read.csv(paste0(root, mydir, 'raw.csv'))
mydata$Tk.locus[mydata$Tk.locus!='+'] = 'Tkmutant'

# plot
ggplot(mydata, aes(Tk.locus, Index, color=Tk.locus, fill=Tk.locus)) +
    theme_classic() +
    theme(legend.position='None') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_color_manual(values=c('+'='black', 'Tkmutant'='blue')) +
    scale_fill_manual(values=c('+'='black', 'Tkmutant'='blue')) +
    geom_hline(yintercept=.5, lty=2) +
    geom_boxplot(alpha=.3) +
    geom_point()
ggsave('index.png', height=4, width=2.2, dpi=300)

# stat
genotype.set = unique(mydata$Tk.locus)
out = NULL
for(g in 1:length(genotype.set)){
    # get data
    mydata.g = subset(mydata, Tk.locus==genotype.set[g])

    # shapiro
    shapiro = mydata.g$Index %>% shapiro.test
    print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'shapiro_',genotype.set[g],'_stat.txt'), row.names=F, col.names=F)

    # t-test or Wilcoxon test
    if(shapiro$p.value < 0.05){
        mytest = wilcox.test(mydata.g$Index)
        effsize = wilcoxonOneSampleRC(mydata.g$Index, mu=0)
    } else{
        mytest = t.test(mydata.g$Index)
        effsize = with(mydata.g, mean(Index)/sd(Index))
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
write.csv(out, paste0(mydir, 'stat_ingeno.csv'), row.names=F)
mystat = t.test(Index ~ Tk.locus, data=mydata)
mystat %>% print %>% capture.output %>% write.table(paste0(mydir, 'stat.txt'), row.names=F, col.names=F)
