
coherence = function(mydata, outdir){
	fnum.thresh = 9000
    ID.set = unique(mydata$ID)
    cl = makeCluster(ncore)
    registerDoParallel(cl)
    coherence = foreach(myID=1:length(ID.set), .combine=rbind, .export = ls(globalenv()), .packages=c('dplyr','seewave','reshape2')) %dopar%{
        subdata = subset(mydata, ID==ID.set[myID], select=c(X,ID_ROI,zscore))
        subdata.dcast = dcast(subdata, formula=X~ID_ROI, value.var='zscore')
		if(nrow(subdata.dcast)>fnum.thresh){
			 comb.set = t(combn(2:ncol(subdata.dcast), 2))
			 lapply(1:nrow(comb.set), function(j){
                        print(j)
				out.j.tmp = ccoh(subdata.dcast[,comb.set[j,1]], subdata.dcast[,comb.set[j,2]], f=1/mydt, wl=fs, plot=F)
				out.j = data.frame(out.j.tmp$coh)
				colnames(out.j) = out.j.tmp$freq
				out.j$time = 1:nrow(out.j)
				out.j$ID_ROI=paste(colnames(subdata.dcast)[comb.set[j,]], collapse='_VS_')
				out.j
			}) %>% bind_rows
		}
    }

    # label phase
	phase = cut(coherence$time, 
                 breaks=seq(min(coherence$time), max(coherence$time)+5, by=5), 
                 include.lowest=T, 
                 rgiht=F, 
                 labels=seq(min(coherence$time), max(coherence$time), by=5))
	coherence$phase = phase %>% as.character %>% as.numeric/5 + 1
    write.csv(coherence, paste0(outdir,'/export/coherence.csv'), row.names=F)
    
    # melt
    coherence.melt = melt(coherence, id.vars=c('phase','ID_ROI','time'), variable.name='period', value.name='power') %>%
        mutate(period = period %>% as.character %>% {gsub('X','',.)} %>% as.numeric %>% {.*1000} %>% round(2))

    # coherence phase mean
    ID_ROI.set = unique(coherence$ID_ROI)
    phase.set = unique(coherence$phase)
    coherence.phasemean = foreach(myID_ROI=1:length(ID_ROI.set), .combine=rbind, .export = ls(globalenv()), .packages=c('dplyr','seewave','reshape2')) %dopar%{
        subdata = subset(coherence, ID_ROI==ID_ROI.set[myID_ROI])
        out = lapply(1:length(phase.set), function(k){
                 subdata.k = subset(subdata, phase==phase.set[k], select=-c(time,phase,ID_ROI))
                 value.mean = apply(subdata.k, 2, function(x){mean(x, na.rm=TRUE)})
                 out.k = data.frame(ID_ROI = ID_ROI.set[myID_ROI],
                                    phase = phase.set[k],
                                    value = value.mean,
                                    frequency = names(value.mean) %>% 
                                        as.character %>% 
                                        as.numeric * 1000)
                 out.k
        }) %>% bind_rows
    }
    write.csv(coherence.phasemean, paste0(outdir,'/export/coherence.phasemean.csv'), row.names=F)

    # coherence time-freq mean
    coherence.mean = meanFreqTime(coherence.melt) # <- defined in spectral.R
    write.csv(coherence.mean, paste0(outdir,'/export/coherence.mean.csv'), row.names=F)
    coherence.mean.norm = normspectrogram(coherence.mean, F,T)
    write.csv(coherence.mean.norm, paste0(outdir,'/export/coherence.mean.norm.csv'), row.names=F)
} 

