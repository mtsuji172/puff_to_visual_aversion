fourieranalysis = function(){
    ID_ROI.set = unique(mydata$ID_ROI)
    targetdt = 5
    cl = makeCluster(ncore)
    registerDoParallel(cl)
    print('generating spectrogram for each ROI, phasewise...')
    fft_phasewise = foreach(k=1:length(ID_ROI.set),
                            .combine=rbind,
                            .export = ls(globalenv()),
                            .packages=c('dplyr','seewave')) %dopar%{
       subdata = subset(mydata, ID_ROI==ID_ROI.set[k])
        subdata.bin = subdata
        bin = cut(subdata$X, 
                 breaks=seq(min(subdata$X), max(subdata$X)+targetdt, by=targetdt), 
                 include.lowest=T, 
                 rgiht=F, 
                 labels=seq(min(subdata$X), max(subdata$X), by=targetdt))
        subdata.bin$phase = paste0('phase',as.numeric(as.character(bin))/5+1)
        subdata.phasewise = split(subdata.bin, f=subdata.bin$phase)
       out = lapply(1:length(subdata.phasewise), function(h){
                x.spec = spectrum(subdata.phasewise[[h]]$zscore, log="no",span=10,plot=FALSE)
                out.h = data.frame(ID_ROI=ID_ROI.set[k],
                           puff=names(subdata.phasewise)[h],
                           density=2*x.spec$spec,
                           frequency=x.spec$freq/mydt)
                out.h
            }) %>% bind_rows()
       out
    }
    fft_phasewise$puff = factor(fft_phasewise$puff, levels=puff.set)
    assign('fft_phasewise', fft_phasewise, envir=.GlobalEnv)
    
    
    # calc mean (and bin freq)
    freq.bin = 0.25
    fft_phasewise.tmp = subset(fft_phasewise[grep('2021', fft_phasewise$ID_ROI), ], condition!='onlyBg')
    if(nrow(fft_phasewise.tmp)==0) next 
    fft_phasewise.tmp$frequency = cut(fft_phasewise.tmp$frequency, 
                                  breaks=seq(freq.bin, max(fft_phasewise.tmp$frequency)+freq.bin, by=freq.bin),
                                  include.lowest=T,
                                  rgiht=F, 
                                  labels=seq(freq.bin, max(fft_phasewise.tmp$frequency), by=freq.bin))
    fft_phasewise.tmp = na.omit(fft_phasewise.tmp)
    freq.set = unique(fft_phasewise.tmp$frequency)
    fft_phasewise.mean = 
        foreach(mypuff=1:length(puff.set),
                .combine=rbind,
                .export = ls(globalenv()),
                .packages='dplyr') %dopar%{
            subdata = subset(fft_phasewise.tmp, puff==puff.set[mypuff])
            out = 
                lapply(1:length(freq.set), function(myfreq){
                 subdata.freq = subset(subdata, frequency==freq.set[myfreq])
                 out = subdata.freq[1,]
                 out$density = mean(subdata.freq$density, na.rm=T)
                 row.names(out)=NULL
                 out
             }) %>% bind_rows()
            out$density = with(out, (density - mean(density)) / sqrt(var(density)))
            out
         }
    stopCluster(cl)
    assign('fft_phasewise.mean', fft_phasewise.mean, envir=.GlobalEnv)
}

spectrogram = function(mydata=mydata, outdir){
    dir.create(outdir, showWarnings=F)
	downrate = 1
    cl = makeCluster(ncore)
    registerDoParallel(cl)
    ID_ROI.set = unique(mydata$ID_ROI)
    spectrogram.power.all = foreach(k=1:length(ID_ROI.set),
                            .combine=rbind,
                            .export = ls(globalenv()),
                            .packages=c('dplyr', 'WaveletComp', 'reshape2')) %dopar%{
        subdata = subset(mydata, ID_ROI==ID_ROI.set[k])
        spectrogram = dplR::morlet(subdata$zscore, x1=seq(min(mydata$X), max(mydata$X),length=length(subdata$zscore)), dj=.1)
        spectrogram.power = spectrogram$Power %>% as.data.frame()
		# downsample
		spectrogram.power = spectrogram.power[seq(1, nrow(spectrogram.power), by=downrate), ]
        colnames(spectrogram.power) = (fs / spectrogram$period) %>% round(3)
        spectrogram.power$time = seq(min(mydata$X), max(mydata$X), length=nrow(spectrogram.power))
		# bin time
		spectrogram.power$time = 
			cut(spectrogram.power$time, 
				 breaks=seq(min(spectrogram.power$time), max(spectrogram.power$time)+targetdt, by=targetdt), 
				 include.lowest=T, 
				 rgiht=F, 
				 labels=seq(min(spectrogram.power$time), max(spectrogram.power$time), by=targetdt))
		time.set = unique(spectrogram.power$time)
		spectrogram.power.bin = 
			lapply(1:length(time.set), function(mytime){
				subdata.mytime = subset(spectrogram.power, time==time.set[mytime])
				if(nrow(subdata.mytime)>1){				
					apply(subdata.mytime %>% as.matrix, 2, max) %>%
						as.list %>%
						data.frame
				} else{
					subdata.mytime
				}
			}) %>% bind_rows()
		# melt        
        spectrogram.power.melt = melt(spectrogram.power.bin, id.vars='time', variable.name='period', value.name='power')
        spectrogram.power.melt$ID_ROI = ID_ROI.set[k]
        spectrogram.power.melt$time = spectrogram.power.melt$time %>% as.character %>% as.numeric
        spectrogram.power.melt$period = spectrogram.power.melt$period %>% as.character %>% {gsub('X','',.)} %>% as.numeric
        spectrogram.power.melt$power = as.numeric(spectrogram.power.melt$power)
        spectrogram.power.melt
    }
    stopCluster(cl)
    spectrogram.power.all$period = as.numeric(as.character(spectrogram.power.all$period))
    spectrogram.power.all$ID_ROI = as.factor(spectrogram.power.all$ID_ROI)
    tmp = str_split_fixed(spectrogram.power.all$ID_ROI, '_', 4)
    spectrogram.power.all$TRIAL = tmp[,3] %>% as.numeric()
    spectrogram.power.all$ID = tmp[,1:2] %>% as.data.frame %>% {do.call(paste, c(., sep='_'))}
    assign('spectrogram.power.all', spectrogram.power.all, envir=.GlobalEnv)
    write.csv(spectrogram.power.all, paste0(outdir,'/spectrogram.power.all.csv'), row.names=F)
    
    # normalize for each neuron (takes < 2h)
    ID_ROI.set = unique(spectrogram.power.all$ID_ROI)
    spectrogram.power.all.norm = 
        lapply(1:length(ID_ROI.set), function(k){
            print(paste0(k, '/', length(ID_ROI.set)))
            subdata = subset(spectrogram.power.all, ID_ROI==ID_ROI.set[k])
            normspectrogram(subdata)
        }) %>% bind_rows
    assign('spectrogram.power.all.norm', spectrogram.power.all.norm, envir=.GlobalEnv)
    write.csv(spectrogram.power.all.norm, paste0(outdir,'/spectrogram.power.all.norm.csv'), row.names=F)

    # get mean for each time/freq
    spectrogram.power.all.mean = meanFreqTime(spectrogram.power.all)
    spectrogram.power.all.mean = normspectrogram(spectrogram.power.all.mean, T, F)
    assign('spectrogram.power.all.mean', spectrogram.power.all.mean, envir=.GlobalEnv)
    write.csv(spectrogram.power.all.mean, paste0(outdir,'/spectrogram.power.all.mean.csv'), row.names=F)
    spectrogram.power.all.mean.norm = normspectrogram(spectrogram.power.all.mean, F, T)
    assign('spectrogram.power.all.mean.norm', spectrogram.power.all.mean.norm, envir=.GlobalEnv)
    write.csv(spectrogram.power.all.mean.norm, paste0(outdir,'/spectrogram.power.all.mean.norm.csv'), row.names=F)
}

