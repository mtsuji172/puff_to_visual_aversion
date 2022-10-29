mydata.spect.merge.dir = 'CaVSmwt/'

prep.Ca.VS.mwt = function(outdir){
    # prep outdir
    dir.create(paste0(outdir,'/',mydata.spect.merge.dir), showWarnings=F)
    
    # read mydata.bin
    mydata.bin = read.csv(paste0(outdir, '/mydata.bin.csv')) %>%
                    subset(select=-c(ROI, dF, uniq))
    colnames(mydata.bin)[colnames(mydata.bin)=='X'] = 'time'
    mydata.bin$ID_ROI_time = with(mydata.bin, paste(ID_ROI, time, sep='_'))
    
    # read mwt.norm
    mwt.norm = readfiles(paste0(outdir, '/mwt_norm/'))
    mwt.norm = melt(mwt.norm, id.vars=c('time','file'), variable.name='period', value.name='power')
    mwt.norm$period = mwt.norm$period %>% as.character %>% {gsub('X','',.)} %>% as.numeric
    mwt.norm$file = mwt.norm$file %>% {gsub('.csv','',.)}
    mwt.norm$ID_ROI_time = with(mwt.norm, paste(file, time, sep='_'))
    
    # merge mydata.bin and mwt.norm, and output, per period
    period.set = unique(mwt.norm$period)
    cl = makeCluster(ncore)
    registerDoParallel(cl)
    foreach(myperiod=1:length(period.set), .combine=rbind, .export = ls(globalenv()), .packages=c('dplyr')) %dopar%{
        mwt.norm.sub = subset(mwt.norm, 
                               period==period.set[myperiod],
                               select=-c(time, file))
        mydata.spect.merge = merge(mydata.bin, mwt.norm.sub, by='ID_ROI_time') %>%
                            subset(select=-ID_ROI_time)
        write.csv(mydata.spect.merge, paste0(outdir, '/',mydata.spect.merge.dir, period.set[myperiod],'Hz.csv'), row.names=F)
    }
    stopCluster(cl)
}

plot.Ca.VS.mwt = function(outdirs, ID_ROI.set, schedules, Hz.min, Hz.max){
    # read relevant files from outdir
    mydata.spect.merge.phase =
        lapply(1:length(outdirs), function(h){
            myfiles = list.files(paste0(outdirs[h], '/', mydata.spect.merge.dir), pattern='.csv$')
            Hz = myfiles %>% {gsub('Hz.csv','',.)} %>% as.numeric
            myfiles.select = myfiles[Hz>=Hz.min & Hz<=Hz.max]
            mydata.spect.merge = lapply(1:length(myfiles.select), function(myfile){
                read.csv(paste0(outdirs[h],'/',mydata.spect.merge.dir, myfiles.select[myfile]))
            }) %>% bind_rows
            
            # subset into phases
            phase.set = names(schedules[[h]])
            mydata.spect.merge.phase.h =
                lapply(1:length(phase.set), function(k){
                   phase.set.k = schedules[[h]][phase.set[k]] %>% unlist
                   subdata =
                       lapply(1:length(phase.set.k), function(h){
                            subset(mydata.spect.merge, time>=phase.set.k[h] & time<(phase.set.k[h]+10))
                        }) %>% bind_rows
                   subdata$phase = phase.set[k]
                   subdata
                 }) %>% bind_rows
            mydata.spect.merge.phase.h
        }) %>% bind_rows

    # plot
    ggplot(subset(mydata.spect.merge.phase, ID_ROI%in%myID_ROI.set), aes(zscore, power)) +
        facet_wrap(.~phase) +
        theme_classic() +
        labs(title=paste0(Hz.min,'-',Hz.max,'Hz')) +
        geom_vline(xintercept=0, lty=2) +
        geom_hline(yintercept=0, lty=2) +
        geom_point(size=.1, alpha=.1) +
        geom_smooth(method='lm')
}
