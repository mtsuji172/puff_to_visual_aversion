mwt = function(mydata=mydata, outdir){
    dir.create(outdir, showWarnings=F)
    dir.create(paste0(outdir,'/mwt'), showWarnings=F)
    cl = makeCluster(ncore)
    registerDoParallel(cl)
    ID_ROI.set = unique(mydata$ID_ROI)
    foreach(k=1:length(ID_ROI.set),
            .combine=rbind,
            .export = ls(globalenv()),
            .packages=c('dplyr', 'WaveletComp', 'reshape2')) %dopar%{
        if(file.exists(paste0(outdir, 'mwt/', ID_ROI.set[k],'.csv'))){
            return()
        }
        subdata = subset(mydata, ID_ROI==ID_ROI.set[k])
        mwt = dplR::morlet(subdata$zscore, x1=seq(min(mydata$X), max(mydata$X),length=length(subdata$zscore)), dj=.1)
        mwt.power = mwt$Power %>% as.data.frame()
        colnames(mwt.power) = (fs / mwt$period) %>% round(5)
        mwt.power$time = seq(min(mydata$X), max(mydata$X), length=nrow(mwt.power))
		
        # bin time
		mwt.power$time = 
			cut(mwt.power$time, 
				 breaks=seq(min(mwt.power$time), max(mwt.power$time)+targetdt, by=targetdt), 
				 include.lowest=T, 
				 rgiht=F, 
				 labels=seq(min(mwt.power$time), max(mwt.power$time), by=targetdt))
        mwt.power.melt = reshape2::melt(mwt.power, id.vars='time', variable.name='period', value.name='value')
        mwt.power.melt$time_period = with(mwt.power.melt, paste(time, period, sep='_'))
        mwt.power.bin =
            mwt.power.melt %>%
            group_by(time_period) %>%
            reframe(time=time[1], ID_ROI=ID_ROI.set[k], period=period[1], value=mean(value)) %>%
            select(-c('time_period'))
        write.csv(mwt.power.bin, paste0(outdir, 'mwt/', ID_ROI.set[k],'.csv'), row.names=F)
    }
    stopCluster(cl)
    
    ### normalize for each neuron
    print('normalizing for each neuron...')
    # prep outdir
    dir.create(paste0(outdir, '/mwt_norm/'))
    myfiles = list.files(paste0(outdir, '/mwt/'), pattern='.csv$')
    lapply(1:length(myfiles), function(k){
        print(paste0(k, '/', length(myfiles)))
        if(!file.exists(paste0(outdir, '/mwt_norm/', myfiles[k]))){
            subdata = read.csv(paste0(outdir, '/mwt/', myfiles[k]))
            subdata = normspectrogram(subdata)
            write.csv(subdata, paste0(outdir, '/mwt_norm/', myfiles[k]), row.names=F)
        } else print('output already exists! skipped.')
    }) %>% bind_rows
}

