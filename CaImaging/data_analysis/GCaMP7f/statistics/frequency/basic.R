meanFreqTime = function(indir, ID_ROI.set=NULL, rmcols){
    # scan files in indir
    myfiles = list.files(indir, pattern='.csv$')

    # select files included in ID_ROI.set
    if(!is.null(ID_ROI.set)){
        if(grep('mwt', indir) %>% any){
            myfiles = myfiles[gsub('.csv','', myfiles) %in% ID_ROI.set]
        } else{ # if wtc, generate X_vs_Y format
            ID_ROI.set.comb =
                apply(expand.grid(ID_ROI.set, ID_ROI.set), 1, paste, collapse="_VS_")
            myfiles = myfiles[gsub('.csv','', myfiles) %in% ID_ROI.set.comb]
        }
    }

    # read the initial file and remove unwanted columns
    out = read.csv(paste0(indir,'/',myfiles[1]))
    out = out[,colnames(out) %ni% rmcols] %>%
        mutate(time.period = paste(time, period, sep='_'))

    # loop over files
    for(k in 2:length(myfiles)){
        print(paste0(k, '/', length(myfiles)))

        # read a file and remove unwanted columns
        mydf = read.csv(paste0(indir,'/',myfiles[k]))
        mydf = mydf[,colnames(mydf) %ni% rmcols] %>%
            mutate(time.period = paste(time, period, sep='_'))

        # for each time, take weighted average of tentative average ('out') and current file
        mydf$value = mydf$value / k
        out$value = out$value * (k-1) / k
        out.next =
            rbind(out, mydf) %>%
            group_by(time.period) %>%
            reframe(time=time[1], period=period[1],value=sum(value))

        # update the tentative average
        out = out.next
    }

    # melt
    out$period = out$period %>% as.character %>% {gsub('X','',.)} %>% as.numeric
    return(out %>% select(-'time.period'))
}

normspectrogram = function(mydf, grandnorm=F, normPerFreq=T){
    if(grandnorm){ # if normalizing the entire set of elements....
        print('implementing grandnorm... ')
        mydf$value = with(mydf, (value - mean(value, na.rm=T)) / sd(value, na.rm=T))
        return(mydf)
    }
    if(normPerFreq){
        mydf.z = 
            mydf %>%
            group_by(period) %>%
            reframe(time=time, ID_ROI=ID_ROI[1], value = (value - mean(value, na.rm=T)) / sd(value, na.rm=T))
        return(mydf.z)
    }
}

# subset myfiles of one fly into phases and get mean
get.mean.phasewise.onefly = function(indir, myID, ok.ID_ROIs, schedule, rmcols, out.postfix=NULL){
	# prep outdir
    outdir = paste0(indir, '/phasemean_flywise/')
    dir.create(outdir, showWarnings=F)

    # read files
	myfiles = list.files(indir, pattern='.csv$')
    myfiles_split = str_split_fixed(myfiles, '_', 5)
    myfiles_ID = paste(myfiles_split[,1], myfiles_split[,2], sep='_')
    if(grep('_VS_', myfiles) %>% any){
        myfiles_ID_ROI = str_split_fixed(myfiles, '_VS_', 2)
        myfiles = myfiles[myfiles_ID==myID & myfiles_ID_ROI[,1]%in%ok.ID_ROIs & gsub('.csv','',myfiles_ID_ROI[,2])%in%ok.ID_ROIs]
    } else{
        myfiles_ID_ROI = gsub('.csv','',myfiles)
        myfiles = myfiles[myfiles_ID==myID & myfiles_ID_ROI %in% ok.ID_ROIs]
    }
    if(length(myfiles)>0){
        mydf = lapply(1:length(myfiles), function(myfile){
                            out = read.csv(paste0(indir, myfiles[myfile]))
                            out[,colnames(out) %ni% rmcols]
                 }) %>% bind_rows
        
        # subset into phases
        phase.set = names(schedule)
        mydf.list =
            lapply(1:length(phase.set), function(k){
               phase.set.k = schedule[[phase.set[k]]]
               subdata = 
                   lapply(1:length(phase.set.k), function(h){
                        subset(mydf, time>=phase.set.k[[h]][1] & time<(phase.set.k[[h]][2]))
                    }) %>% bind_rows
             }) %>% c
        names(mydf.list) = phase.set

        # get mean for each phase and save
        for(k in 1:length(mydf.list)){
            mydf.list[[k]]$time = mydf.list[[k]]$time %% 10
            time.set = unique(mydf.list[[k]]$time)
            mydf.list.mean =
                lapply(1:length(time.set), function(myt){
                    subdata = subset(mydf.list[[k]], time==time.set[myt], select=-time)
                    out = apply(subdata, 2, mean)
                    out$time = time.set[myt]
                    out
                 }) %>% bind_rows
            outname = ifelse(is.null(out.postfix),
                             paste0(outdir,myID,'_',phase.set[k],'.csv'),
                             paste0(outdir,myID,'_',phase.set[k],'_',out.postfix,'.csv'))
            write.csv(mydf.list.mean, outname, row.names=F)
        }
    } else
        print('no file passed! skipping')
}

# subset myfile into phases and get mean
get.mean.phasewise.onedir = function(indir, ID_ROI.set=NULL, schedule, rmcols, out.postfix=NULL){
	# prep outdir
    outdir = paste0(indir, '/phasemean/')
    dir.create(outdir, showWarnings=F)

    # read files
	myfiles = list.files(indir, pattern='.csv$')
    if(!is.null(ID_ROI.set)){
		# if intercellular index, then take combinations wherein both members are posi
		if(grep('_VS_', myfiles[1]) %>% length > 0){
			myfiles_trim = myfiles %>% strsplit('_VS_') %>% unlist
			myfiles_trim.1 = myfiles_trim[-grep('.csv', myfiles_trim)]
			myfiles_trim.2 = myfiles_trim[grep('.csv', myfiles_trim)] %>% {gsub('.csv','',.)}
			myfiles = myfiles[(myfiles_trim.1 %in% ID_ROI.set) & (myfiles_trim.2 %in% ID_ROI.set)]
		} else{
			myfiles = myfiles[gsub('.csv','', myfiles) %in% ID_ROI.set]
		}
	}
    mydf = lapply(1:length(myfiles), function(myfile){
                out = read.csv(paste0(indir, myfiles[myfile]))
                out[,colnames(out) %ni% rmcols]
            }) %>% bind_rows
    
    # subset into phases
    phase.set = names(schedule)
	mydf.list =
        lapply(1:length(phase.set), function(k){
           phase.set.k = schedule[[phase.set[k]]] %>% unlist
           subdata = subset(mydf, time>=phase.set.k[1] & time<(phase.set.k[2]))
         }) %>% c
    names(mydf.list) = phase.set

    # get mean for each phase and save
	for(k in 1:length(mydf.list)){
		mydf.list[[k]]$time = mydf.list[[k]]$time %% 10
        time.set = unique(mydf.list[[k]]$time)
		mydf.list.mean =
            lapply(1:length(time.set), function(myt){
                subdata = subset(mydf.list[[k]], time==time.set[myt], select=-time)
                out = apply(subdata, 2, mean)
                out$time = time.set[myt]
                out
             }) %>% bind_rows
        outname = ifelse(is.null(out.postfix),
                         paste0(outdir,phase.set[k],'.csv'),
                         paste0(outdir,phase.set[k],'_',out.postfix,'.csv'))
		write.csv(mydf.list.mean, outname, row.names=F)
	}
}

get.mean.phasewise.acrossdir = function(indirs, postfix = NULL, outname){
    
    # get phase-wise mean
    phase.set = names(myschedule)
    phasemean = 
        lapply(1:length(phase.set), function(myphase){
            # read files
            myfiles = 
                lapply(1:length(indirs), function(mydir){
                    myfiles.sub = list.files(indirs[mydir], pattern='*.csv')
                    if(is.null(postfix)){
                        if(file.exists(paste0(indirs[mydir], '/', phase.set[myphase], '.csv'))){
                            read.csv(paste0(indirs[mydir], '/', phase.set[myphase], '.csv'))
                        }
                    } else{
                        if(file.exists(paste0(indirs[mydir], '/', phase.set[myphase], '_', postfix, '.csv'))){
                            read.csv(paste0(indirs[mydir], '/', phase.set[myphase], '_', postfix, '.csv'))
                        }
                    }
                 }) %>% bind_rows
            time.set = unique(myfiles$time) %>% sort
            out = lapply(1:length(time.set), function(myt){
                             subdata =subset(myfiles, time==time.set[myt])
                             apply(subdata, 2, function(x) mean(x, na.rm=T))
                 }) %>% bind_rows
            out$phase = phase.set[myphase]
            out
        }) %>% bind_rows
    phasemean.melt  = melt(phasemean, id.vars=c('time','phase'), 
                           variable.name = 'period', value.name = 'power')
    phasemean.melt$period = phasemean.melt$period %>% as.character %>% {gsub('X','',.)} %>% as.numeric
    if(readline(prompt=paste0("period range is currently: ",min(phasemean.melt$period),"-",max(phasemean.melt$period),". Needs translating into Hz? type F or T\n"))){
        print('translating...')
        phasemean.melt$period = 1 / phasemean.melt$period # translate into Hz
    } else print('translation skipped')

    # write
    write.csv(phasemean.melt, paste0(outname), row.names=F)
    assign('phasemean', phasemean.melt, envir=.GlobalEnv)
}


calcBandMean = function(indir, band.set){
    # list files
    myfiles = list.files(path=indir, pattern='.csv$')

    cl = makeCluster(ncore)
    registerDoParallel(cl)
    
	mydf.bandmean = 
        lapply(1:length(band.set), function(myband){
            mydf.band =
                lapply(1:length(myfiles), function(myfile){
                    print(paste0(myfile, '/', length(myfiles)))

                    # read file
                    mydf = read.csv(paste0(indir, myfiles[myfile]))
                    mydf = subset(mydf, period>=band.set[[myband]][1] & period<=band.set[[myband]][2])

                    # calc mean/time
                    time.set = mydf$time %>% unique
                    out = foreach(mytime=1:length(time.set), .combine=rbind, .export = c(ls(globalenv()),'myband'), .packages=c('dplyr')) %dopar%{
                        subdata = subset(mydf, time==time.set[mytime])
                        out.time = subdata[1,]
                        out.time$power = mean(subdata$power, na.rm=T)
                        out.time$period = paste(band.set[[myband]], collapse='to')
                        out.time
                    }

                    # output
                    out$ID_ROI = gsub('.csv', '', myfiles[myfile])
                    out
                }) %>% bind_rows
            mydf.band
        }) %>% bind_rows
    stopCluster(cl)
	return(mydf.bandmean)
}

calcBandMean.phasewise = function(indirs, band.set, outdir, ID_ROI.set=NULL, zscore=F){
    # prep outdir
    dir.create(outdir, showWarnings=F)

    # for each indir...
    for(indir in 1:length(indirs)){
        # list files
        myfiles = list.files(path=indirs[indir], pattern='.csv$')
        myfiles.full = paste0(indirs[indir], '/', myfiles)

        # loop over files
        lapply(1:length(myfiles.full), function(myfile){
            print(paste0(myfile, '/', length(myfiles.full)))

            # if neurons not included in ID_ROI.set (as per some user-defined conditions), skip
            if(!is.null(ID_ROI.set)){
                tmp = myfiles[myfile] %>% {strsplit(., '_VS_')} %>% unlist %>% {gsub('.csv','',.)}
                if((tmp[1] %ni% ID_ROI.set) | (tmp[1] %ni% ID_ROI.set)){
                    print(paste0(myfiles[myfile],' not a combination included in provided list of ID_ROIs! skipped'))
                    return()
                }
            }

            # if file already exists, skip
            if(file.exists(paste0(outdir, '/', myfiles[myfile]))){
                print(paste0(myfiles[myfile],' already exists! skipped'))
                return()
            }

            # read file
            mydf = read.csv(myfiles.full[myfile])
            if(grep('ID_ROI', colnames(mydf)) %>% any)
                mydf = subset(mydf, select=-ID_ROI)

            # value of each freq into zscore (if zscore=T)
            if(zscore){
                for(i in 1:ncol(mydf)){
                    if(colnames(mydf)[i]!='time'){
                        mydf[,i] = (mydf[,i] - mean(mydf[,i])) / sd(mydf[,i])
                    }
                }
            }

            # select periods
            mydf = subset(mydf, period>=band.set[1] & period<=band.set[2])

            # subset into phases
            myschedule = myschedules[[indir]]
            phase.set = names(myschedule)
            mydf.list =
                lapply(1:length(phase.set), function(k){
                    # get data
                   phase.set.k = myschedule[[phase.set[k]]] %>% unlist
                   subdata = subset(mydf, time>=phase.set.k[1] & time<(phase.set.k[2]))

                   # translate time to 0~10s
                   subdata$time = subdata$time %% 10
                   subdata
                 }) %>% c
            names(mydf.list) = phase.set

            # calc mean/time/phase and combine
            myperiod = paste(band.set, collapse='to')
            myID_ROI = gsub('.csv', '', myfiles[myfile])
            time.set = mydf$time %>% unique
            out = 
                lapply(1:length(mydf.list), function(p){
                    mydf = mydf.list[[p]]
                    out.phase = 
                        mydf %>%
                        group_by(time) %>%
                        reframe(value = mean(value, na.rm=T)) %>%
                        mutate(period = myperiod, phase = names(myschedule)[p], ID_ROI=myID_ROI)
                    out.phase
                 }) %>% bind_rows

            # output
            write.csv(out, paste0(outdir, '/', myfiles[myfile]))
        })
    }
}

periodgram = function(mydf){
    ggplot(mydf, aes(time, period, fill=value)) + 
        theme_classic() +
        scale_y_continuous(breaks = 2^(seq(-7,7,by=.5)) %>% round(2), trans='log2') +
        geom_tile(height=.2) +
        scale_fill_gradientn(colors=jet.colors(7)) %>%
        return
}

get.change = function(myband.sets, myID_ROI.set, path2csv){
    bandmean.ID_ROImean.diff = 
        lapply(1:length(myband.sets), function(k){
            # set myband.set
            myband.set = myband.sets[[k]]

            # set path
            myoutdir = paste0(path2csv,paste(myband.set, collapse='to'),'/')

            # import results
            myfiles = list.files(myoutdir)
            myfiles = myfiles[gsub('.csv','',myfiles) %in% myID_ROI.set]
            bandmean =
                lapply(1:length(myfiles), function(f){
                    read.csv(paste0(myoutdir,myfiles[f]))
                }) %>% bind_rows

            # mean / cell of bandmean for initial 1.5s of phases
            bandmean.ID_ROImean.k = 
                bandmean %>%
                subset(time<1.5) %>%
                mutate(ID_ROI_phase = paste(ID_ROI, phase, sep='_')) %>%
                group_by(ID_ROI_phase) %>%
                reframe(period=period[1], ID_ROI=ID_ROI[1], phase=phase[1], value = mean(value)) %>%
                select(-'ID_ROI_phase')

            # calc effect / cell
            bandmean.ID_ROImean.k.diff =
                bandmean.ID_ROImean.k %>%
                group_by(ID_ROI) %>%
                reframe(period=bandmean.ID_ROImean.k$period[1], visual.base = value[phase=='visual.base'] - value[phase=='base'], base.puff = value[phase=='base.puff'] - value[phase=='base'], visual.puff = value[phase=='visual.puff'] - value[phase=='base'])

            # out
            bandmean.ID_ROImean.k.diff %>% reshape2::melt(id.vars = c('ID_ROI','period'), variable.name = 'phase', value.name='value')
        }) %>% bind_rows

    bandmean.ID_ROImean.diff = subset(bandmean.ID_ROImean.diff, phase %in% c('base.puff','visual.base','visual.puff'))
    bandmean.ID_ROImean.diff$period = factor(bandmean.ID_ROImean.diff$period, levels=myband.sets_name)
    bandmean.ID_ROImean.diff$phase = factor(bandmean.ID_ROImean.diff$phase, levels=c('base.puff','visual.base','visual.puff'))
    bandmean.ID_ROImean.diff$date = str_split_fixed(bandmean.ID_ROImean.diff$ID_ROI, '_', 2)[,1]

    return(bandmean.ID_ROImean.diff)
}
