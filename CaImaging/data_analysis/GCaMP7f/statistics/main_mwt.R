##############################
# load libaries
##############################

source('init.R')
source('basic.R')
source('frequency/basic.R')
source('frequency/mwt.R')
source('CaVSmwt.R')

##############################
# params
##############################
outdir = outdirs[1]
myschedule = myschedules[[1]]
myband.sets = list(c(1,4), c(4,8), c(8,16), c(16,32))
myband.sets_name = sapply(1:length(myband.sets), function(b){ paste(myband.sets[[b]], collapse='to') })
path2out = paste0(path2data, 'mwt_bandmean_out/')


##############################
# read data
##############################

# list dirs
mydirs = 
    lapply(1:length(outdirs), function(k){
       paste0(list.dirs(path=gsub('export/','', outdirs[k]), recursive=FALSE), '/GminusR/aligned/')
    }) %>% c %>% unlist
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
# average periodgram  (mwt_mean_posi for forward; mwt_mean_select_posi for reverse)
##################################

ID_ROI.posi = subset(posinega, profile=='posi')$ID_ROI %>% unique
ID_ROI.nega = subset(posinega, profile=='nega')$ID_ROI %>% unique

mydf = meanFreqTime(indir=paste0(outdir,'mwt/'), ID_ROI.set, rmcols='ID_ROI')
write.csv(mydf, paste0(outdir, 'mwt_mean_select_posi.csv'), row.names=F)

mydf = read.csv(paste0(outdir,'mwt_mean_posi_cellthresh1.csv'))
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

    calcBandMean.phasewise(indirs = paste0(outdirs, 'mwt/'),
                           band.set = myband.set,
                           outdir = myoutdir,
                           zscore = F)
}

# calculate per-cell changes from baseline per band
bandmean.ID_ROImean.diff = get.change(myband.sets, ID_ROI.posi, path2csv = path2out)

n.fly.tmp = str_split_fixed(bandmean.ID_ROImean.diff$ID_ROI, '_', 3)[,1:2]
n.fly = paste(n.fly.tmp[,1], n.fly.tmp[,2], sep='_') %>% unique %>% length %>% print
n.cell = unique(bandmean.ID_ROImean.diff$ID_ROI) %>% length %>% print
nsize = data.frame(n.fly, n.cell)
write.csv(nsize, paste0(path2out, 'nsize.csv'), row.names=F)

# plot
bandmean.ID_ROImean.diff.F = subset(bandmean.ID_ROImean.diff, date %in% subset(schedule.date, schedule=='F')$date)
bandmean.ID_ROImean.diff.R = subset(bandmean.ID_ROImean.diff, date %in% subset(schedule.date, schedule=='R')$date)
ggplot(bandmean.ID_ROImean.diff, aes(phase, value)) +
    facet_grid(.~period) +
    theme_classic() +
    theme(legend.position='None') +
    geom_hline(yintercept=0) +
    stat_summary(col='blue')
ggsave(paste0(path2out, 'bandmean.ID_ROImean.diff.png'), height=3, width=5, dpi=300)

# save raw traces
write.csv(select(bandmean.ID_ROImean.diff, c('period','phase','ID_ROI','value')), paste0(path2out, 'bandmean.ID_ROImean.diff_raw.csv'), row.names=F)
write.csv(select(bandmean.ID_ROImean.diff.F, c('period','phase','ID_ROI','value')), paste0(path2out, 'bandmean.ID_ROImean.diff.F_raw.csv'), row.names=F)
write.csv(select(bandmean.ID_ROImean.diff, c('period','phase','ID_ROI','value')), paste0(path2out, 'bandmean.ID_ROImean.diff_raw.csv'), row.names=F)

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



########################################
# Ca2+ response vs theta response
########################################
bandmean.ID_ROImean.diff =
    get.change(myband.sets, unique(posinega$ID_ROI), path2out)  %>%
    subset(period=='4to8' & phase=='visual.puff')

theta.vs.FR =
    lapply(1:nrow(bandmean.ID_ROImean.diff), function(r){
       bandmean.ID_ROImean.diff.r = bandmean.ID_ROImean.diff[r,]
       posinega.r = subset(posinega, ID_ROI==bandmean.ID_ROImean.diff.r$ID_ROI)
       if(nrow(posinega.r)>0){
           bandmean.ID_ROImean.diff.r$Ca = subset(posinega.r, phase%in%c('puff','puff.previsual'))$zscore %>% mean
           bandmean.ID_ROImean.diff.r
       } else{
           print('error')
       }
    }) %>% bind_rows
theta.vs.FR$CaLabel = '-'
theta.vs.FR$CaLabel[theta.vs.FR$Ca>1] = 'p'
ggplot(theta.vs.FR, aes(CaLabel, value, color=CaLabel)) + 
    theme_classic() +
    theme(legend.position='None') +
    scale_color_manual(values=c('p'='red','-'='black')) +
    geom_hline(yintercept=0) +
    stat_summary(size=1)
ggsave(paste0(path2out, 'theta.vs.FR.png'), height=3, width=1.5, dpi=300)
write.csv(select(theta.vs.FR, c('CaLabel','value')), paste0(path2out, 'theta.vs.FR_raw.csv'), row.names=F)

# stat
CaLabel.set = unique(theta.vs.FR$CaLabel)
out = NULL
for(p in 1:length(CaLabel.set)){
    theta.vs.FR.p = subset(theta.vs.FR, CaLabel==CaLabel.set[p])
    mytest = t.test(theta.vs.FR.p$value)
    effsize = with(theta.vs.FR.p, mean(value)/sd(value))
    mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
    colnames(mytest.df) = c('method','parameter','statistic','p.value')
    out.sub = data.frame(CaLabel = CaLabel.set[p], mytest.df, effsize)
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
write.csv(out, paste0(path2out, 'theta.vs.FR_stat_perCaLabel.csv'), row.names=F)

mystat = t.test(value ~ CaLabel, data=theta.vs.FR)
mystat %>% print %>% capture.output %>% write.table(paste0(path2out, 'theta.vs.FR_stat.txt'), row.names=F, col.names=F)
