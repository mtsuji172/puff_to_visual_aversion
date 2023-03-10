##############################
# load libaries
##############################

source('init_bar.R')
source('basic.R')
source('frequency/basic.R')
source('frequency/mwt.R')

##############################
# params
##############################
myband.sets = list(c(1,4), c(4,8), c(8,16), c(16,32))
myband.sets_name = sapply(1:length(myband.sets), function(b){ paste(myband.sets[[b]], collapse='to') })
path2out = paste0(outdir, 'mwt_bandmean_out/')

##############################
# read data
##############################

# list dirs
mydirs = paste0(list.dirs(path=gsub('export/','', outdir), recursive=FALSE), '/GminusR/aligned/')
mydirs = mydirs[-grep('export', mydirs)]
mydirs = mydirs[grep('ID', mydirs)]
mydirs = gsub('//','/', mydirs)

# load result
mydata = 
    lapply(1:length(mydirs), function(k){
       read.csv(paste0(mydirs[k], 'mydata.csv'))
    }) %>% bind_rows


##################################
# calc morlet wavelet transform (mwt) for each neuron
##################################
mwt(mydata, outdir)

##################################
# average periodgram 
##################################
# get ID_ROI.posi of thresh 1 (new!)
posinega_ori = posinega
ID_ROI.set = unique(posinega_ori$ID_ROI)
posinega = 
    lapply(1:length(ID_ROI.set), function(k){
          subdata = subset(posinega_ori, ID_ROI==ID_ROI.set[k])
          subdata$profile = ifelse(mean(subdata$zscore[subdata$phase%in%c('puff','puff.previsual')])>1, 'posi', 'nega')
          subdata
    }) %>% bind_rows
ID_ROI.posi = subset(posinega, profile=='posi')$ID_ROI %>% unique

### periodgram of average over select flies
mydf = meanFreqTime(indir=paste0(outdir,'mwt/'), ID_ROI.set=NULL, rmcols='ID_ROI')
write.csv(mydf, paste0(outdir, 'mwt_mean_all.csv'), row.names=F)
mydf = read.csv(paste0(outdir,'/mwt_mean_all.csv'))

png(paste0(outdir, 'mwt_mean_posi_4-32Hz.png'), height=2, width=6, units='in', res=300)
periodgram(mydf %>% subset(time!=0 & period>=4 & period<32)) + theme_classic() + geom_vline(xintercept=seq(10,70,by=10)-.25, col='white') + theme(text=element_text(size=15))
dev.off()
png(paste0(outdir, 'mwt_mean_posi_0.5-4Hz.png'), height=1, width=6, units='in', res=300)
periodgram(subset(mydf, period>=.5 & period<4)) + theme_classic() + geom_vline(xintercept=seq(10,70,by=10)-.25, col='white') + theme(text=element_text(size=15))
dev.off()


########################################
# Bandmean of mwt
########################################

# calulate average band powers per band per cell
for(myband.set in myband.sets){
    myoutdir = paste0(path2out, paste(myband.set, collapse='to'),'/')
    dir.create(myoutdir, showWarnings=F, recursive=T)

    calcBandMean.phasewise(indirs = paste0(outdir, 'mwt/'),
                           band.set = myband.set,
                           outdir = myoutdir,
                           zscore = F)
}

# calculate per-cell changes from baseline per band
bandmean.ID_ROImean.diff = get.change(myband.sets, ID_ROI.posi, path2out) %>% subset(period=='4to8')
write.csv(select(bandmean.ID_ROImean.diff %>% subset(period=='4to8'), c('period','phase','ID_ROI','value')), paste0(path2out, 'bandmean.ID_ROImean.diff_raw.csv'), row.names=F)

n.fly.tmp = str_split_fixed(bandmean.ID_ROImean.diff$ID_ROI, '_', 3)[,1:2]
n.fly = paste(n.fly.tmp[,1], n.fly.tmp[,2], sep='_') %>% unique %>% length %>% print
n.cell = unique(bandmean.ID_ROImean.diff$ID_ROI) %>% length %>% print
nsize = data.frame(n.fly, n.cell)
print(nsize)
write.csv(nsize, paste0(path2out, 'nsize.csv'), row.names=F)

# plot
ggplot(bandmean.ID_ROImean.diff, aes(phase, value)) +
    facet_grid(.~period) +
    theme_classic() +
    theme(legend.position='None') +
    geom_hline(yintercept=0) +
    ylim(c(-3.5, 1.5)) +
    stat_summary(col='blue')
ggsave(paste0(path2out, 'bandmean.ID_ROImean.diff.png'), height=3, width=1.5, dpi=300)

# stat (within genotype)
bandmean.ID_ROImean.diff$period_phase = with(bandmean.ID_ROImean.diff, paste(period, phase, sep='_'))
period_phase.set = unique(bandmean.ID_ROImean.diff$period_phase)
out = NULL
for(p in 1:length(period_phase.set)){
    bandmean.ID_ROImean.diff.p = subset(bandmean.ID_ROImean.diff, period_phase==period_phase.set[p])
    mytest = t.test(bandmean.ID_ROImean.diff.p$value)
    effsize = with(bandmean.ID_ROImean.diff.p, mean(value)/sd(value))
    mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
    colnames(mytest.df) = c('method','parameter','statistic','p.value')
    out.sub = data.frame(period_phase = period_phase.set[p], mytest.df, effsize)
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
write.csv(out, paste0(path2out, 'bandmean.ID_ROImean.diff_inperiod-phase.csv'), row.names=F)

# calc difference / cell (visual.puff vs others)
bandmean.ID_ROImean.diff_visual.puff = 
    bandmean.ID_ROImean.diff %>%
    mutate(ID_ROI_period = paste(ID_ROI, period, sep='_')) %>%
    group_by(ID_ROI_period) %>%
    mutate(visual.puff.value = value[phase=='visual.puff']) %>%
    reframe(ID_ROI=ID_ROI[1], period=period[1], phase=phase, period_phase=paste(period, phase, sep='_'), date=date[1], value = visual.puff.value - value) %>%
    select(-'ID_ROI_period') %>%
    subset(phase != 'visual.puff')

# stat
period_phase.set = unique(bandmean.ID_ROImean.diff_visual.puff$period_phase)
out = NULL
for(p in 1:length(period_phase.set)){
    bandmean.ID_ROImean.diff_visual.puff.p = subset(bandmean.ID_ROImean.diff_visual.puff, period_phase==period_phase.set[p])
    mytest = t.test(bandmean.ID_ROImean.diff_visual.puff.p$value)
    effsize = with(bandmean.ID_ROImean.diff_visual.puff.p, mean(value)/sd(value))
    mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
    colnames(mytest.df) = c('method','parameter','statistic','p.value')
    out.sub = data.frame(period_phase = period_phase.set[p], mytest.df, effsize)
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
write.csv(out, paste0(path2out, 'bandmean.ID_ROImean.diff_vs-visual.puff.csv'), row.names=F)
