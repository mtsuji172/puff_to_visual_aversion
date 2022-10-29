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

##############################
# read data
##############################

# list dirs
mydirs = 
    lapply(1:length(outdirs), function(k){
       paste0(list.dirs(path=gsub('export/','', outdirs[k]), recursive=FALSE), '/GminusR/aligned/')
    }) %>% c %>% unlist
mydirs = mydirs[-grep('export', mydirs)]

# preprocessing
mydirs = mydirs[grep('ID', mydirs)]
mydirs = gsub('//','/', mydirs)

# load result
mydata = 
    lapply(1:length(mydirs), function(k){
       read.csv(paste0(mydirs[k], 'mydata.csv'))
    }) %>% bind_rows
puffeffect = 
    lapply(1:length(mydirs), function(k){
       if(file.exists(paste0(mydirs[k], 'puffeffect.csv')))
           read.csv(paste0(mydirs[k], 'puffeffect.csv'))
    }) %>% bind_rows
tmp = puffeffect$ID_ROI %>% str_split_fixed('_',4)
puffeffect$ID = paste(tmp[,1], tmp[,2], sep='_')

##################################
# calc morlet wavelet transform (mwt) for each neuron
##################################
mwt(mydata, outdir)

##################################
# average periodgram 
##################################
### periodgram of average over select flies
periodgram_mean_select(outdir=outdir, target='mwt', select.unit='neuron', mythresh=1)

mydf = read.csv(paste0(outdir,'/mwt_mean_posi.csv'))
periodgram(subset(mydf, time!=0 & period>=3 & period<32)) + theme_classic() + geom_vline(xintercept=seq(10,70,by=10)-.25, col='white') + theme(text=element_text(size=15))

### remove "puff response" from the time course (for b-p-v-b-p-b-v-b)
mydf_base = subset(mydf, time<10)

# extract puff window (puff.previsual)
mydf_puff = subset(mydf, ((time>=myschedule$puff[[1]][1]) & (time<myschedule$puff[[1]][2])) | ((time>=myschedule$puff.previsual[[1]][1]) & (time<myschedule$puff.previsual[[1]][2])))
mydf_puff$time = mydf_puff$time %% 10

# average puff windows
period.set = unique(mydf_puff$period)
time.set = unique(mydf_puff$time)
mydf_puff_ave =
    lapply(1:length(time.set), function(t){
        lapply(1:length(period.set), function(p){
               subdata = subset(mydf_puff, time==time.set[t] & period==period.set[p])
               out = subdata[1,]
               out$power = subdata$power %>% mean
               out
           }) %>% bind_rows
      }) %>% bind_rows

# extract "puff response" as mydf_puff_ave - mydf_base
puff.response = 
    lapply(1:length(time.set), function(t){
        lapply(1:length(period.set), function(p){
               puff.sub = subset(mydf_puff_ave, time==time.set[t] & period==period.set[p])
               base.sub = subset(mydf_base, time==time.set[t] & period==period.set[p])
               out = puff.sub
               out$power = puff.sub$power - base.sub$power
               out
           }) %>% bind_rows
      }) %>% bind_rows

# subtract "puff response" from puff windows
mydf_noPuff = 
    subset(mydf, 
           !( ((time>=myschedule$puff[[1]][1]) & (time<myschedule$puff[[1]][2])) |
             ((time>=myschedule$puff.previsual[[1]][1]) & (time<myschedule$puff.previsual[[1]][2])) )
           )

mydf_puff1_subtracted = subtract_puff(myschedule$puff[[1]])
mydf_puff2_subtracted = subtract_puff(myschedule$puff.previsual[[1]])
mydf_final = rbind(mydf_noPuff, mydf_puff1_subtracted, mydf_puff2_subtracted)

periodgram(subset(mydf_final, time>.5 & period>2.5 & period<32)) + geom_vline(xintercept=seq(0,70,by=10)-.25, col='white')  # -> crop period > 3 later as image


########################################
# Bandmean of mwt
########################################

# select neurons of flies with positive puff responses and without suffering from sarine leakage
outdirs = outdir
mythresh = 1
tmp = str_split_fixed(puffeffect$ID_ROI, '_', 4)
puffeffect$ID = paste(tmp[,1],tmp[,2], sep='_')
myID_ROI.set = with(subset(puffeffect, ID%in%ID.sarineOK),
                    ID_ROI[zscore[phase=='puff.previsual'] >= mythresh]) %>% unique
print(paste0('found ', length(myID_ROI.set), ' neurons'))

# initial setups
myband.set = c(4,8)
myoutdir = paste0('../../data/mwt_bandmean_out_test/mwt_norm_bandmean_',paste(myband.set, collapse='to'),'/')

# calculate bandmean
calcBandMean.phasewise(indirs = paste0(outdirs, '/mwt_norm/'),
                       band.set = myband.set,
                       outdir = myoutdir,
                       zscore = F)

# loop over bandsets and calculate per-cell average power and its differences from baseline
myband.sets = list(c(1,4), c(4,8), c(8,16), c(16,32))
bandmean.ID_ROImean = NULL
bandmean.ID_ROImean.diff = NULL
for(k in 1:length(myband.sets)){
    # set myband.set
    myband.set = myband.sets[[k]]

    # set path
    myoutdir = paste0('../../data/mwt_bandmean_out/mwt_norm_bandmean_',paste(myband.set, collapse='to'),'/')

    # import results
    myfiles = list.files(myoutdir)
    myfiles = lapply(1:length(myfiles), function(f){
                 if(gsub('.csv','', myfiles[f]) %in% myID_ROI.set){
                     myfiles[f]
                 }
             }) %>% c %>% unlist
    bandmean = lapply(1:length(myfiles), function(myfile){
                      print(paste0(myoutdir,'/',myfiles[myfile]))
                      read.csv(paste0(myoutdir,'/',myfiles[myfile]))
                }) %>% bind_rows

    # initial 1s is omitted from base -> time = time - 1
    bandmean$time[bandmean$phase=='base'] = bandmean$time[bandmean$phase=='base'] - 1
    tmp = bandmean$ID_ROI %>% str_split_fixed('_',4)
    bandmean$ID = paste(tmp[,1], tmp[,2], sep='_')
    bandmean$phase = factor(bandmean$phase, levels=names(myschedules[[1]]))
    phase.set = c('base','visual.base','base.puff','visual.puff')

    # mean / cell of bandmean within a specified time window
    myID_ROI.set = unique(bandmean$ID_ROI)
    bandmean.ID_ROImean.k = 
        lapply(1:length(myID_ROI.set), function(myID_ROI){
           subdata.ID_ROI = subset(bandmean, ID_ROI==myID_ROI.set[myID_ROI] & time<1.5)
           lapply(1:length(phase.set), function(myphase){
                   subdata.ID_ROI.phase = subset(subdata.ID_ROI, phase==phase.set[myphase])
                   out = subdata.ID_ROI.phase[1,]
                   out$power = mean(subdata.ID_ROI.phase$power, rm.na=T)
                   out
             }) %>% bind_rows
        }) %>% bind_rows %>% na.omit

    # calc difference / cell (base vs all)
    myID_ROI.set = unique(bandmean.ID_ROImean.k$ID_ROI)
    bandmean.ID_ROImean.diff.k =
        lapply(1:length(myID_ROI.set), function(myID_ROI){
               subdata.ID_ROI = subset(bandmean.ID_ROImean.k, ID_ROI==myID_ROI.set[myID_ROI])
               out = subdata.ID_ROI[1:3,]
               out$power = c(
                             get_index(subset(subdata.ID_ROI, phase=='visual.base')$power,
                                        subset(subdata.ID_ROI, phase=='base')$power),
                             get_index(subset(subdata.ID_ROI, phase=='visual.puff')$power,
                                        subset(subdata.ID_ROI, phase=='base')$power),
                             get_index(subset(subdata.ID_ROI, phase=='base.puff')$power,
                                        subset(subdata.ID_ROI, phase=='base')$power)
                            )
               out$index = c('visual.base','visual.puff','base.puff')
               out
        }) %>% bind_rows

    # out
    bandmean.ID_ROImean = rbind(bandmean.ID_ROImean, bandmean.ID_ROImean.k)
    bandmean.ID_ROImean.diff = rbind(bandmean.ID_ROImean.diff, bandmean.ID_ROImean.diff.k)
}

# plot
bandmean.ID_ROImean.diff$period = factor(bandmean.ID_ROImean.diff$period, levels=c('1to4','4to8','8to16','16to32'))
ggplot(bandmean.ID_ROImean.diff, aes(index, power)) +
    facet_grid(.~period) +
    theme_classic() +
    theme(legend.position='None') +
    geom_hline(yintercept=0) +
    stat_summary(col='blue')

# stat
phase.set = c('visual.base','base.puff','visual.puff')
out = 
    lapply(1:length(phase.set), function(p){
        p.value = t.test(subset(bandmean.ID_ROImean.diff, index==phase.set[p])$power)$p.value
        data.frame(index=phase.set[p], p.value = p.value)
    }) %>% bind_rows
out$p.value = p.adjust(out$p.value, method='fdr')

# calc difference / cell (visual.puff vs others)
bandmean.ID_ROImean.diff_visual.puff =
    lapply(1:length(myID_ROI.set), function(k){
       subdata.ID_ROI = subset(bandmean.ID_ROImean, ID_ROI==myID_ROI.set[k])
       period.set = unique(subdata.ID_ROI$period)
       lapply(1:length(period.set), function(p){
           subdata.ID_ROI.p = subset(subdata.ID_ROI, period==period.set[p])
           out = subdata.ID_ROI.p[1:2,]
           out$period = period.set[p]
           out$power = c(
                         get_index(subset(subdata.ID_ROI.p, phase=='visual.puff')$power,
                                    subset(subdata.ID_ROI.p, phase=='base.puff')$power),
                         get_index(subset(subdata.ID_ROI.p, phase=='visual.puff')$power,
                                    subset(subdata.ID_ROI.p, phase=='visual.base')$power)
                        )
           out$index = c('visual.puff-vs-base.puff','visual.puff-vs-visual.base')
           out
        }) %>% bind_rows
    }) %>% bind_rows

# stat
myperiod = '4to8'
out = NULL
for(myIndex in bandmean.ID_ROImean.diff_visual.puff$index %>% unique){
    tmp = subset(bandmean.ID_ROImean.diff_visual.puff, period==myperiod)
    pvalue = t.test(with(tmp, power[index==myIndex]))$p.value
    out = rbind(out, data.frame(phase=myIndex, pvalue))
}
out$pvalue = p.adjust(out$pvalue, method='bonferroni')

########################################
# Ca2+ response vs theta response
########################################
tmp = subset(bandmean.ID_ROImean.diff, period=='4to8' & index=='visual.puff', select=-c(X, time))
tmp2 =
    lapply(1:nrow(tmp), function(r){
       tmp.r = tmp[r,]
       posinega.r = subset(posinega, ID_ROI==tmp.r$ID_ROI)
       if(nrow(posinega.r)>0){
           tmp.r$Ca = subset(posinega.r, phase%in%c('puff','puff.previsual'))$zscore %>% mean
           tmp.r
       } else{
           print('error')
       }
    }) %>% bind_rows
tmp2$CaLabel = '-'
tmp2$CaLabel[tmp2$Ca>1] = 'p'
ggplot(tmp2, aes(CaLabel, power, color=CaLabel)) + 
    theme_classic() +
    theme(legend.position='None') +
    scale_color_manual(values=c('p'='red','-'='black')) +
    geom_hline(yintercept=0) +
    stat_summary(size=1)
mystat = t.test(power ~ CaLabel, data=tmp2)
