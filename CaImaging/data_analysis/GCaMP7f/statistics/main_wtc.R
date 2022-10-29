##############################
# load libaries
##############################

source('init.R')
source('basic.R')
source('frequency/basic.R')
source('frequency/wtc.R')
source('CaVSwtc.R')

# param
outdir = outdirs[1]

##################################
# read files
##################################
mydata = 
    lapply(1:length(outdirs), function(k){
    }) %>% bind_rows

##################################
# calc wavelet coherence (wtc)
##################################
for(outdir in outdirs){
    print(outdir)
    mydata = read.csv(paste0(outdir, 'mydata.csv'))
    wtc.calc(mydata, outdir)
}

##################################
# average periodgram 
##################################
### periodgram of average over select flies (can handle single dir ("outdir") only)
periodgram_mean_select(outdir=outdir, target='wtc', select.unit='fly', mythresh=1)

mydf = read.csv(paste0(outdir,'/wtc_mean_posi_thresh1.csv'))
mydf$period = mydf$period %>% round(3)
png(paste0('./plots/wtc_mean_b-v-b-p-b-p-v-b_thresh1.png'), height=3, width=6, units='in', res=300)
periodgram(subset(mydf, period>=3 & period<32)) +
    geom_vline(xintercept=seq(10,70,by=10)-.25, col='white')
dev.off()

##################################
# phase-wise periodgram of average over select flies 
##################################
# calculate and save
outdirs = outdir
mythresh = 1
for(myprofile in c('posi')){
    print(paste0('[I] processing ',myprofile))
    for(x in 1:length(outdirs)){
        print(paste0('[II] processing ', outdirs[x]))

        # list dirs
        mydirs = paste0(list.dirs(path=gsub('export/','', outdirs[x]), recursive=FALSE))
        mydirs = mydirs[-grep('export', mydirs)]

        # preprocessing
        mydirs = mydirs[grep('ID', mydirs)]
        mydirs = gsub('//','/', mydirs)

        # get list of ID_ROI
        myID_ROI.set = 
            lapply(1:length(mydirs), function(k){
                       mydirs[k] %>% strsplit('/') %>% unlist %>% tail(3) %>% head(1)
                   }) %>% c %>% unlist

        print(paste0('found ', length(myID_ROI.set), ' neurons'))
        if(length(myID_ROI.set)>0){
            get.mean.phasewise.onedir(indir = paste0(outdirs[[x]],'/wtc_norm/'),
                                      ID_ROI.set = myID_ROI.set,
                                      schedule = myschedules[[x]],
                                      rmcols = 'ID_ROI',
                                      out.postfix = myprofile
                                      )
        }
    }
    get.mean.phasewise.acrossdir(indirs = paste0(outdirs, '/wtc_norm/phasemean/'),
                                 postfix = myprofile,
                                 outname = paste0('../../data/export_acrossdir/wtc_norm.phasemean.',myprofile,'.csv'))
}
# plot posi- vs nega- responders
phasemean = read.csv('../../data/export_acrossdir/wtc.phasemean.posi.csv')
phasemean.norm = normspectrogram(phasemean, F, T)
phasemean.norm$phase = factor(phasemean.norm$phase %>% {gsub('_posi','',.)} %>% {gsub('_nega','',.)}, levels=names(myschedules[[1]]))
periodgram(subset(phasemean, period>3 & period<32))  + facet_wrap(.~phase, ncol=length(phase.set)*2)

########################################
# Bandmean of wtc
########################################

# initial setups
myband.set = c(4,8)
myoutdir = paste0('../../data/wtc_bandmean_out/wtc_bandmean_',paste(myband.set, collapse='to'),'/')

# calculate bandmean
calcBandMean.phasewise(indirs= paste0(outdirs, '/wtc_norm/'),
                        band.set=myband.set,
                        outdir=myoutdir,
                        ID.set = ID.posi)

# loop over bandsets
myband.sets = list(c(1,4), c(4,8), c(8,16), c(16,32))
bandmean.ID_ROImean.diff.all =
    lapply(1:length(myband.sets), function(k){
        # set myband.set
        myband.set = myband.sets[[k]]

        # set path
        myoutdir = paste0('../../data/wtc_bandmean_out/wtc_bandmean_',paste(myband.set, collapse='to'),'/')

        # import results
        myfiles = list.files(myoutdir)
        bandmean = lapply(1:length(myfiles), function(myfile){
                          read.csv(paste0(myoutdir,'/',myfiles[myfile]))
                    }) %>% bind_rows
        bandmean$phase = factor(bandmean$phase, levels=names(myschedules[[1]]))

        # mean / cell of bandmean for initial 1s of phases
        myID_ROI.set = unique(bandmean$ID_ROI)
        bandmean.ID_ROImean = 
            lapply(1:length(myID_ROI.set), function(k){
                   subdata.ID_ROI = subset(bandmean, ID_ROI==myID_ROI.set[k] & time<1.5)
                   lapply(1:length(phase.set), function(myphase){
                           subdata.ID_ROI.phase = subset(subdata.ID_ROI, phase==phase.set[myphase])
                           out = subdata.ID_ROI.phase[1,]
                           out$power = mean(subdata.ID_ROI.phase$power, rm.na=T)
                           out
                     }) %>% bind_rows
            }) %>% bind_rows

        # calc effect / cell
        myID_ROI.set = unique(bandmean.ID_ROImean$ID_ROI)
        bandmean.ID_ROImean.diff =
            lapply(1:length(myID_ROI.set), function(k){
                   subdata.ID_ROI = subset(bandmean.ID_ROImean, ID_ROI==myID_ROI.set[k])
                   out = subdata.ID_ROI[1:3,]
                   out$power = c(
                                 subset(subdata.ID_ROI, phase=='visual.base')$power - 
                                    subset(subdata.ID_ROI, phase=='base')$power,
                                 subset(subdata.ID_ROI, phase=='base.puff')$power - 
                                    subset(subdata.ID_ROI, phase=='base')$power,
                                 subset(subdata.ID_ROI, phase=='visual.puff')$power - 
                                    subset(subdata.ID_ROI, phase=='base')$power
                                )
                   out$index = c('visual.base', 'base.puff','visual.puff')
                   out
            }) %>% bind_rows

        # out
        bandmean.ID_ROImean.diff
    }) %>% bind_rows

# plot
bandmean.ID_ROImean.diff.all$period = factor(bandmean.ID_ROImean.diff.all$period, levels=c('1to4','4to8','8to16','16to32'))
ggplot(bandmean.ID_ROImean.diff.all, aes(index, power)) +
    facet_wrap(.~period, scale='free', nrow=1) +
    theme_classic() +
    theme(legend.position='None') +
    geom_hline(yintercept=0) +
    stat_summary(col='blue')


# stat
myperiod = '4to8'
out = NULL
bandmean.ID_ROImean.diff.sub = subset(bandmean.ID_ROImean.diff.all, period==myperiod)
for(myIndex in bandmean.ID_ROImean.diff.sub$index %>% unique){
    out =
        rbind(out,
            data.frame(
                       phase=myIndex,
                       pvalue = t.test(with(bandmean.ID_ROImean.diff.sub, power[index==myIndex]))$p.value
                       )
        )
}
out$pvalue = p.adjust(out$pvalue, method='fdr')

# stat
myID_ROI.set = bandmean.ID_ROImean.diff.sub$ID_ROI %>% unique
bandmean.ID_ROImean.diff2 =
    lapply(1:length(myID_ROI.set), function(k){
           subdata.ID_ROI = subset(bandmean.ID_ROImean.diff.sub, ID_ROI==myID_ROI.set[k])
           out = subdata.ID_ROI[1:2,]
           out$power = c(
                         subset(subdata.ID_ROI, index=='visual.puff')$power - 
                            subset(subdata.ID_ROI, index=='base.puff')$power,
                         subset(subdata.ID_ROI, index=='visual.puff')$power - 
                            subset(subdata.ID_ROI, index=='visual.base')$power
                        )
           out$index = c('base.puff','visual.base')
           out
    }) %>% bind_rows
out = NULL
for(myIndex in bandmean.ID_ROImean.diff2$index %>% unique){
    out =
        rbind(out,
            data.frame(
                       phase=myIndex,
                       pvalue = t.test(with(bandmean.ID_ROImean.diff2, power[index==myIndex]))$p.value
                       )
        )
}
out$pvalue = p.adjust(out$pvalue, method='fdr')
print(out)
write.csv(out, './plots/wtc_bandmean_4to8_phasewise_diff2_stat.csv', row.names=F)


##################################
# [Ca2+] vs mwt/wtc <- both binned per 0.5s
##################################
for(k in 1:length(outdirs))
    prep.Ca.VS.mwt(outdirs[k])
myID_ROI.set = subset(posinega, ID%in%ID.sarineOK & profile=='posi')$ID_ROI %>% unique
plot.Ca.VS.mwt(outdirs, ID_ROI.set=myID_ROI.set, myschedules, Hz.min=4, Hz.max=8)

myID_ROI.set = subset(bandmean, (ID_ROI.1 %in% ID_ROI.sarineOK.posi) | (ID_ROI.2 %in% ID_ROI.sarineOK.posi))$ID_ROI %>% unique
for(k in 1:length(outdirs))
    prep.Ca.VS.wtc(outdirs[k])
plot.Ca.VS.wtc(outdirs, schedules, Hz.min=4, Hz.max=8)

}
