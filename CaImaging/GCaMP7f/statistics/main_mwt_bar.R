##############################
# load libaries
##############################

source('init.R')
source('basic.R')
source('frequency/basic.R')
source('frequency/mwt.R')

##############################
# params
##############################
outdir = '../../data/jGCaMP7f_152Hz_b-v-b-p-b-p-v-b_bar/'
myschedule = myschedules[[1]]

##############################
# read data
##############################

# list dirs
mydirs = 
       paste0(list.dirs(path=gsub('export/','', outdir), recursive=FALSE), '/GminusR/aligned/')
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
png(paste0('./plots/mwt_mean_b-v-b-p-b-p-v-b.png'), height=3, width=6, units='in', res=300)
periodgram(subset(mydf, time!=0 & period>=3 & period<32)) + theme_classic() + geom_vline(xintercept=seq(10,70,by=10)-.25, col='white') + theme(text=element_text(size=15))
dev.off()

### remove "puff response" from the time course (for b-v-b-p-b-p-v-b)
# extract baseline
phasemean = read.csv('../../data/export_acrossdir/mwt.phasemean.posi.csv')
mydf_base_phasemean = subset(phasemean, phase=='base')

# extract puff window (puff.previsual)
mydf_puff = subset(mydf, ((time>=myschedule$puff.previsual[[1]][1]) & (time<myschedule$puff.previsual[[1]][2])))
mydf_puff$time = mydf_puff$time %% 10

# extract "puff response" as mydf_puff - mydf_base
period.set = unique(mydf_puff$period)
time.set = unique(mydf_puff$time)
puff.response = 
    lapply(1:length(time.set), function(t){
        lapply(1:length(period.set), function(p){
               puff.sub = subset(mydf_puff, time==time.set[t] & period==period.set[p])
               base.sub = subset(mydf_base, time==time.set[t] & period==period.set[p])
               out = puff.sub
               out$power = puff.sub$power - base.sub$power
               out
           }) %>% bind_rows
      }) %>% bind_rows
periodgram(subset(mydf_base, period>=2.5 & period<32 & time>.5))
periodgram(subset(mydf_puff, period>=2.5 & period<32 & time>.5))
periodgram(subset(puff.response, period>=2.5 & period<32 & time>.5))

# subtract "puff response" from puff windows
mydf_noPuff = 
    subset(mydf, 
           !((time>=myschedule$puff[[1]][1]) & (time<myschedule$puff[[1]][2])))

mydf_puff1_subtracted = subtract_puff(myschedule$puff[[1]])
mydf_puff2_subtracted = subtract_puff(myschedule$puff.previsual[[1]])
mydf_final = rbind(mydf_noPuff, mydf_puff1_subtracted, mydf_puff2_subtracted)

periodgram(subset(mydf_final, time>.5 & period>2.5 & period<32)) + geom_vline(xintercept=seq(0,70,by=10)-.25, col='white')


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

# select neurons of flies with positive puff responses
mythresh = 1
myID_ROI.set = with(puffeffect, ID_ROI[zscore[phase=='puff.previsual'] >= mythresh]) %>% unique
print(paste0('found ', length(myID_ROI.set), ' neurons'))

# initial setups
myband.set = c(4,8)
myoutdir = paste0('../../data/bar_mwt_bandmean_out/mwt_norm_bandmean_',paste(myband.set, collapse='to'),'/')

# calculate bandmean
calcBandMean.phasewise(indirs = paste0(outdir, '/mwt_norm/'),
                       band.set = myband.set,
                       outdir = myoutdir,
                       zscore = F)

# import results
myfiles = list.files(myoutdir)
myfiles = lapply(1:length(myfiles), function(f){
             if(gsub('.csv','', myfiles[f]) %in% myID_ROI.set){
                 myfiles[f]
             }
         }) %>% c %>% unlist
bandmean = lapply(1:length(myfiles), function(f){
                  file_content = read.csv(paste0(myoutdir,myfiles[f]))
                  if(nrow(file_content)>0)
                      file_content
            }) %>% bind_rows

# initial 1s with strong artefactual noise is omitted from base -> time = time - 1
bandmean$time[bandmean$phase=='base'] = bandmean$time[bandmean$phase=='base'] - 1
tmp = bandmean$ID_ROI %>% str_split_fixed('_',4)
bandmean$ID = paste(tmp[,1], tmp[,2], sep='_')
bandmean$phase = factor(bandmean$phase, levels=names(myschedules[[1]]))
phase.set = c('base','visual.base','base.puff','visual.puff')

# mean / cell of bandmean within a specified time window
myID_ROI.set = unique(bandmean$ID_ROI)
bandmean.ID_ROImean = 
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
myID_ROI.set = unique(bandmean.ID_ROImean$ID_ROI)
bandmean.ID_ROImean.diff =
    lapply(1:length(myID_ROI.set), function(myID_ROI){
           subdata.ID_ROI = subset(bandmean.ID_ROImean, ID_ROI==myID_ROI.set[myID_ROI])
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

# plot
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
summary(as.factor(tmp2$CaLabel))
ggplot(tmp2, aes(CaLabel, power, color=CaLabel)) + 
    theme_classic() +
    theme(legend.position='None') +
    scale_color_manual(values=c('p'='red','-'='black')) +
    geom_hline(yintercept=0) +
    stat_summary(size=1)
mystat = t.test(power ~ CaLabel, data=tmp2)
print(mystat)
