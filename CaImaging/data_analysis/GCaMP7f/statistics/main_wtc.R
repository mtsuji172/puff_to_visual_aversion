##############################
# load libaries
##############################

source('init.R')
source('basic.R')
source('frequency/basic.R')
source('frequency/wtc.R')

# param
outdir = outdirs[1]
path2out = paste0(path2data, 'wtc_bandmean_out/')
myband.sets = list(c(1,4), c(4,8), c(8,16), c(16,32))

##################################
# calc wavelet coherence (wtc)
##################################
for(outdir in outdirs) wtc.calc(outdir)

##################################
# average periodgram 
##################################

ID_ROI.posi = subset(posinega, profile=='posi')$ID_ROI %>% unique
mydf = meanFreqTime(indir=paste0(outdir,'wtc_norm/'), ID_ROI.set = ID_ROI.posi, rmcols='ID_ROI')
write.csv(mydf, paste0(outdir, 'wtc_norm_mean_posi_cellthresh1.csv'), row.names=F)

mydf = read.csv(paste0(outdir,'wtc_mean_posi_thresh1.csv'))
mydf$period = mydf$period %>% round(3)
png(paste0(outdir, 'wtc_mean_b-v-b-p-b-p-v-b_thresh1_log2.png'), height=3, width=6, units='in', res=300)
periodgram(subset(mydf, time>1 & period>=4 & period<32)) +
    geom_vline(xintercept=seq(10,70,by=10)-.25, col='white')
dev.off()


########################################
# Bandmean of wtc
########################################

# initial setups
for(myband in myband.sets){
    myoutdir = paste0(path2out, paste(myband, collapse='to'),'/')
    dir.create(myoutdir, recursive=T, showWarnings=F)

    # calculate bandmean
    calcBandMean.phasewise(indirs= paste0(outdirs, '/wtc_norm/'),
                            band.set=myband,
                            outdir=myoutdir,
                            ID_ROI.set = NULL)
}
print('')

## loop over bandsets
bandmean = NULL
bandmean.ID_ROImean = NULL
bandmean.ID_ROImean.diff.all = NULL
for(k in 1:length(myband.sets)){
    # set myband.set
    myband.set = myband.sets[[k]]

    # set path
    myoutdir = paste0(path2out, paste(myband.set, collapse='to'),'/')

    # import results
    myfiles = list.files(myoutdir)
    bandmean.k =
        lapply(1:length(myfiles), function(myfile){
            read.csv(paste0(myoutdir,'/',myfiles[myfile]))
        }) %>% bind_rows
    bandmean = rbind(bandmean, bandmean.k)

    # mean / cell of bandmean for initial 1.5s of phases
    bandmean.ID_ROImean.k = 
        bandmean.k %>%
        subset(time<1.5) %>%
        mutate(ID_ROI_phase = paste(ID_ROI, phase, sep='_')) %>%
        group_by(ID_ROI_phase) %>%
        reframe(period=period[1], ID_ROI=ID_ROI[1], phase=phase[1], value = mean(value)) %>%
        select(-'ID_ROI_phase')
    bandmean.ID_ROImean = rbind(bandmean.ID_ROImean, bandmean.ID_ROImean.k)

    # calc effect / cell
    ID_ROI.set = unique(bandmean.ID_ROImean.k$ID_ROI)
    phase.set = unique(bandmean.ID_ROImean.k$phase)
    phase.set = phase.set[phase.set != 'base']
    cl = makeCluster(ncore)
    registerDoParallel(cl)
    bandmean.ID_ROImean.diff.k = 
        foreach(i=1:length(ID_ROI.set), .combine=rbind, .export = c(ls(globalenv())), .packages=c('dplyr')) %dopar%{
            # get data
            bandmean.i = subset(bandmean.k, ID_ROI==ID_ROI.set[i])
            bandmean.ID_ROImean.i = subset(bandmean.ID_ROImean.k, ID_ROI==ID_ROI.set[i])

            # get schedule
            mydate = bandmean.ID_ROImean.i[1,'ID_ROI'] %>% as.character %>% strsplit('_') %>% unlist %>% head(1)
            schedule.type = subset(schedule.date, date==mydate)$schedule
            myschedule.i = myschedules[[schedule.type]]
            myschedule_order.i = myschedule_orders[[schedule.type]]

            # main
            lapply(1:length(phase.set), function(p){
                preceding = with(bandmean.i, max(X[X < unlist(myschedule.i[[phase.set[p]]])[1]]))
                precedingvalue = subset(bandmean.i, X==preceding)$value
                out.p = subset(bandmean.ID_ROImean.i, phase==phase.set[p])[1,]
                out.p$value = out.p$value - precedingvalue
                out.p
             }) %>% bind_rows
         }
    stopCluster(cl)
    bandmean.ID_ROImean.diff.all = rbind(bandmean.ID_ROImean.diff.all, bandmean.ID_ROImean.diff.k)
}
write.csv(bandmean.ID_ROImean.diff.all, paste0(path2out, 'bandmean.ID_ROImean.diff.all.csv'), row.names=F)
bandmean.ID_ROImean.diff.all = read.csv(paste0(path2out, 'bandmean.ID_ROImean.diff.all.csv'))

bandmean.ID_ROImean.diff = subset(bandmean.ID_ROImean.diff.all, phase %in% c('base.puff','visual.base','visual.puff'))
bandmean.ID_ROImean.diff$period = factor(bandmean.ID_ROImean.diff$period, levels=c('1to4','4to8','8to16','16to32'))
bandmean.ID_ROImean.diff$phase = factor(bandmean.ID_ROImean.diff$phase, levels=c('base.puff','visual.base','visual.puff'))
bandmean.ID_ROImean.diff$date = str_split_fixed(bandmean.ID_ROImean.diff$ID_ROI, '_', 2)[,1]


write.csv(bandmean.ID_ROImean.diff %>% select(-'date'), paste0(path2out, 'bandmean.ID_ROImean.diff_raw.csv'), row.names=F)

n.fly.tmp = str_split_fixed(bandmean.ID_ROImean.diff$ID_ROI, '_', 3)[,1:2]
n.fly = paste(n.fly.tmp[,1], n.fly.tmp[,2], sep='_') %>% unique %>% length %>% print
n.cell = str_split_fixed(bandmean.ID_ROImean.diff$ID_ROI, '_VS_', 2) %>% as.vector %>% unique %>% length %>% print
nsize = data.frame(n.fly, n.cell)
write.csv(nsize, paste0(path2out, 'nsize.csv'), row.names=F)

# plot
ggplot(bandmean.ID_ROImean.diff, aes(phase, value)) +
    facet_wrap(.~period, nrow=1) +
    theme_classic() +
    theme(legend.position='None') +
    geom_hline(yintercept=0) +
    stat_summary(col='blue')
ggsave(paste0(path2out, 'coherence_change.png'), height=4, width=8.5, dpi=300)

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
