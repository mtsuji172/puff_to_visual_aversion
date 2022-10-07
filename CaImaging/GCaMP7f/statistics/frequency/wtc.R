library(biwavelet)

wtc.calc = function(mydata, outdir){
    dir.create(paste0(outdir, '/wtc/'), showWarnings=F)
    ID.set = unique(mydata$ID)
    cl = makeCluster(ncore)
    registerDoParallel(cl)
	lapply(1:length(ID.set), function(myID){
		print(paste0(ID.set[myID], ": ", myID, '/', length(ID.set)))
		subdata = subset(mydata, ID==ID.set[myID], select=c(X,ID_ROI,zscore))
		subdata.dcast = dcast(subdata, formula=X~ID_ROI, value.var='zscore')
		comb.set = t(combn(2:ncol(subdata.dcast), 2))

        # for each pair
        foreach(j=1:nrow(comb.set), .combine=rbind, .export = ls(globalenv()), .packages=c('dplyr','biwavelet','reshape2')) %dopar%{
            # define file name of the output
            ID_ROI=paste(colnames(subdata.dcast)[comb.set[j,]], collapse='_VS_')
            outname = paste0(outdir, '/wtc/', ID_ROI, '.csv')

            # if outfile already exists, return
            if(file.exists(outname)){
                return(NULL)
            }

            # calc wavelet coherence
            out.j.tmp = wtc(data.frame(x=seq(1,nrow(subdata.dcast))*mydt, y=subdata.dcast[,comb.set[j,1]]), 
                            data.frame(x=seq(1,nrow(subdata.dcast))*mydt, y=subdata.dcast[,comb.set[j,2]]), 
                            nrands=0)
            out.j = out.j.tmp$power %>% t %>% as.data.frame
            colnames(out.j) = out.j.tmp$period
            out.j$time = out.j.tmp$t
            out.j$ID_ROI = ID_ROI
            
            # bin time
            out.j$time = 
                cut(out.j$time, 
                     breaks=seq(0, sessionDuration+targetdt, by=targetdt), 
                     include.lowest=T, 
                     rgiht=F, 
                     labels=seq(0, sessionDuration, by=targetdt))
            time.set = unique(out.j$time)
            out.j.bin = lapply(1:length(time.set), function(myt){
                subdata = subset(out.j, time==time.set[myt])
                subdata.out = 
                    apply(subset(subdata, select=-c(time,ID_ROI)) %>% as.matrix, 2, mean) %>% 
                        as.list %>%
                        as.data.frame
                data.frame(subdata.out, subdata[1,c('time','ID_ROI')])
            }) %>% bind_rows				
            write.csv(out.j.bin, outname, row.names=F)
        }
	})
    stopCluster(cl)
    
    ### normalize for each neuron (takes < 2h)
    print('normalizing signals of each neuron...')

    # prep outdir
    dir.create(paste0(outdir, '/wtc_norm/'))
    myfiles = list.files(paste0(outdir, '/wtc/'), pattern='.csv$')
    cl = makeCluster(ncore)
    registerDoParallel(cl)
    lapply(1:length(myfiles), function(k){
        print(paste0(k, '/', length(myfiles)))
        # if outfile non-existent, calculate
        if(!file.exists(paste0(outdir, '/wtc_norm/', myfiles[k]))){
            subdata = read.csv(paste0(outdir, '/wtc/', myfiles[k]))
            subdata = melt(subdata, id.vars=c('time', 'ID_ROI'), variable.name='period', value.name='power')
            subdata$period = subdata$period %>% as.character %>% {gsub('X','',.)} %>% as.numeric
            subdata = normspectrogram(subdata,makecluster=F) %>%
                {dcast(., time~period, value.var='power')}
            write.csv(subdata, paste0(outdir, '/wtc_norm/', myfiles[k]), row.names=F)
        } else{
            print('output already exists! skipped.')
        }
    }) %>% bind_rows
    stopCluster(cl)
}

meandiff = function(test){
    mean(subset(test, LRcoarse=='L')$power) - mean(subset(test, LRcoarse=='R')$power)
}

resamplingLR = function(test, nshuffle){
    meandiff.obs =  meandiff(test)
    meandiff.shuffle =
        lapply(1:nshuffle, function(k){
                   test.copy = test
                   test.copy$LRcoarse = sample(test.copy$LRcoarse, size=length(test.copy$LRcoarse), replace=F)
                   meandiff(test.copy)
         }) %>% c %>% unlist
    pvalue = max(sum(meandiff.shuffle >= meandiff.obs) / nshuffle, 1/nshuffle)
    return(pvalue)
}
