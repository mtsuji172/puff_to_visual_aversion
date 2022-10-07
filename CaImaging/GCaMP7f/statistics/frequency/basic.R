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
            print('ID_ROI.set.comb:')
            head(ID_ROI.set.comb) %>% print
            print('myfiles:')
            head(myfiles) %>% print
            myfiles = myfiles[gsub('.csv','', myfiles) %in% ID_ROI.set.comb]
        }
    }

    # read the initial file and remove unwanted columns
    out = read.csv(paste0(indir,'/',myfiles[1]))
    out = out[,colnames(out) %ni% rmcols]

    # loop over files
    for(k in 2:length(myfiles)){
        print(paste0(k, '/', length(myfiles)))

        # read a file and remove unwanted columns
        mydf = read.csv(paste0(indir,'/',myfiles[k]))
        mydf = mydf[,colnames(mydf) %ni% rmcols]

        # for each time, take weighted average of tentative average ('out') and current file
        time.set = unique(out$time)
        out.next =
            lapply(1:length(time.set), function(mytime){
                subdata = subset(mydf, time==time.set[mytime])
                subout = subset(out, time==time.set[mytime])
                 if(nrow(subdata)==1){
                     # weighted average
                     (subout*(k-1) + subdata) / k
                 }
             }) %>% bind_rows

        # update the tentative average
        out = out.next
    }

    # melt
    out = melt(out, id.vars='time', variable.name='period', value.name='power')
    out$period = out$period %>% as.character %>% {gsub('X','',.)} %>% as.numeric
    return(out)
}

normspectrogram = function(mydf, grandnorm=F, normPerFreq=T, makecluster=T){
    if(grandnorm){ # if normalizing the entire set of elements....
        print('implementing grandnorm... ')
        mydf$power = with(mydf, (power - mean(power, na.rm=T)) / sqrt(var(power, na.rm=T)))
        return(mydf)
    }
    if(normPerFreq){
        period.set = unique(mydf$period)
        if(makecluster){
            cl = makeCluster(ncore)
            registerDoParallel(cl)
        }
        mydf.mean = 
            foreach(myperiod=1:length(period.set),
                    .combine=rbind,
                    .export = ls(globalenv()),
                    .packages='dplyr') %dopar%{
                subdata = subset(mydf, period==period.set[myperiod])
                subdata$power = with(subdata, (power - mean(power, na.rm=T)) / sqrt(var(power, na.rm=T)))
                subdata
             }
        if(makecluster) stopCluster(cl)
        return(mydf.mean)
    }
}

periodgram_mean_select = function(outdir=outdir, target='mwt', select.unit='fly', mythresh=1){ # or 'neuron'
    # read posinega (thresh=0), and update its thresh to mythresh
    posinega_ori = read.csv(paste0(outdir,'/posinega.csv'))

    # select flies with no sarine leakage
    posinega_ori = subset(posinega_ori, ID%in%ID.sarineOK)
    
    # update posinega with current "mythresh"
    ID.set = unique(posinega_ori$ID)
    posinega = 
        lapply(1:length(ID.set), function(k){
              subdata = subset(posinega_ori, ID==ID.set[k])
              subdata$profile = ifelse(mean(subdata$zscore[subdata$phase%in%c('puff','puff.previsual')])>mythresh,
                                          'posi',
                                          'nega')
              subdata
        }) %>% bind_rows

    # process posi- nega- responders separately
    for(myprofile in c('posi')){
        print(paste0('processing ', myprofile, ' flies...'))

        # diverge according to "select.unit"
        if(select.unit=='fly'){
            myID_ROI.set = subset(posinega, profile==myprofile)$ID_ROI %>% unique
        }
        if(select.unit=='neuron'){
            mydirs = paste0(list.dirs(path=gsub('export/','', outdir), recursive=FALSE), '/GminusR/aligned/')
            mydirs = mydirs[grep('ID', mydirs)]
            mydirs = gsub('//','/', mydirs)
            mydata.flyROImean = 
                lapply(1:length(mydirs), function(k){
                           read.csv(paste0(mydirs[k], 'mydata.flyROImean.csv'))
                }) %>% bind_rows
            tmp = str_split_fixed(mydata.flyROImean$ID_ROI, '_', 4)
            mydata.flyROImean$ID = paste(tmp[,1],tmp[,2], sep='_')
            if(myprofile=='posi')
                myID_ROI.set = with(mydata.flyROImean,
                                    ID_ROI[zscore[phase=='puff.previsual'] >= mythresh]) %>% unique
            else
                myID_ROI.set = with(mydata.flyROImean,
                                    ID_ROI[zscore[phase=='puff.previsual'] <= -mythresh]) %>% unique
        }
        print(paste0('found ', length(myID_ROI.set), ' neurons'))
        print('# e.g. ############')
        print(head(myID_ROI.set))
        print('##################')
        if(length(myID_ROI.set)>0){
            out = meanFreqTime(indir=paste0(outdir,'/',target,'/'), ID_ROI.set = myID_ROI.set, rmcols='ID_ROI')
            write.csv(out, paste0(outdir,'/',target,'_mean_', myprofile,'.csv'), row.names=F)
        }
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
        mydf.list = lapply(1:length(phase.set), function(k){
                               phase.set.k = schedule[[phase.set[k]]]
                               subdata = lapply(1:length(phase.set.k), function(h){
                                                    subset(mydf, time>=phase.set.k[[h]][1] & time<(phase.set.k[[h]][2]))
                                }) %>% bind_rows
                     }) %>% c
        names(mydf.list) = phase.set

        # get mean for each phase and save
        for(k in 1:length(mydf.list)){
            mydf.list[[k]]$time = mydf.list[[k]]$time %% 10
            time.set = unique(mydf.list[[k]]$time)
            mydf.list.mean = lapply(1:length(time.set), function(myt){
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
	mydf.list = lapply(1:length(phase.set), function(k){
                           phase.set.k = schedule[[phase.set[k]]]
                           subdata = lapply(1:length(phase.set.k), function(h){
                                                subset(mydf, time>=phase.set.k[[h]][1] & time<(phase.set.k[[h]][2]))
                            }) %>% bind_rows
             }) %>% c
    names(mydf.list) = phase.set

    # get mean for each phase and save
	for(k in 1:length(mydf.list)){
		print(k)
		mydf.list[[k]]$time = mydf.list[[k]]$time %% 10
        time.set = unique(mydf.list[[k]]$time)
		mydf.list.mean = lapply(1:length(time.set), function(myt){
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
                    mydf = melt(mydf, id.var='time', variable.name = 'period', value.name='power')
                    mydf$period = mydf$period %>% {gsub('X','',.)} %>% as.character %>% as.numeric
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


calcBandMean.phasewise = function(indirs, band.set, outdir, ID.set=NULL, zscore=F){
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

            # if neuron of flies not included in ID.set (as per some user-defined conditions), skip
            if(!is.null(ID.set)){
                tmp = myfiles[myfile] %>% {strsplit(., '_')} %>% unlist
                tmp2 = paste(tmp[1:2], collapse='_')
                if(tmp2 %ni% ID.set){
                    print(paste0(myfiles[myfile],' not included in provided list of ID! skipped'))
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

            # power of each freq into zscore (if zscore=T)
            if(zscore){
                for(i in 1:ncol(mydf)){
                    if(colnames(mydf)[i]!='time'){
                        mydf[,i] = (mydf[,i] - mean(mydf[,i])) / sqrt(var(mydf[,i]))
                    }
                }
            }

            # melt
            mydf = melt(mydf, id.var='time', variable.name = 'period', value.name='power')
            mydf$period = mydf$period %>% {gsub('X','',.)} %>% as.character %>% as.numeric

            # select periods
            mydf = subset(mydf, period>=band.set[1] & period<=band.set[2])

            # subset into phases
            myschedule = myschedules[[indir]]
            phase.set = names(myschedule)
            mydf.list =
                lapply(1:length(phase.set), function(k){
                   phase.set.k = myschedule[[phase.set[k]]]
                   subdata = 
                       lapply(1:length(phase.set.k), function(h){
                            subset(mydf, time>=phase.set.k[[h]][1] & time<(phase.set.k[[h]][2]))
                        }) %>% bind_rows
                   # translate time to 0~10s
                   subdata$time = subdata$time %% 10
                   subdata
                 }) %>% c
            names(mydf.list) = phase.set

            # calc mean/time/phase and combine
            time.set = mydf$time %>% unique
            out = 
                lapply(1:length(mydf.list), function(myphase){
                    mydf = mydf.list[[myphase]]
                    out.phase = 
                        lapply(1:length(time.set), function(mytime){
                            subdata = subset(mydf, time==time.set[mytime])
                            if(nrow(subdata)>0){
                                out.time = subdata[1,]
                                out.time$power = mean(subdata$power, na.rm=T)
                                out.time$period = paste(band.set, collapse='to')
                                out.time
                            }
                        }) %>% bind_rows
                    out.phase$phase = names(myschedule)[myphase]
                    out.phase
                 }) %>% bind_rows

            # output
            out$ID_ROI = gsub('.csv', '', myfiles[myfile])
            write.csv(out, paste0(outdir, '/', myfiles[myfile]))
        })
    }
}

periodgram = function(mydf){
    ggplot(mydf, aes(time, period, fill=power)) + 
        theme_classic() +
        scale_y_continuous(breaks = 2^(seq(-7,7,by=.5)) %>% round(2), trans='log2') +
        geom_tile(height=.2) +
        scale_fill_gradientn(colors=jet.colors(7)) %>%
        return
}

subtract_puff = function(myschedule.puff){
    mydf_puff = subset(mydf, (time>=myschedule.puff[1]) & (time<myschedule.puff[2]))
    puff.response.tmp = puff.response
    puff.response.tmp$time = puff.response.tmp$time + myschedule.puff[1]
    time.set = unique(mydf_puff$time)
    mydf_puff_subtracted =
        lapply(1:length(time.set), function(t){
            lapply(1:length(period.set), function(p){
                   mydf_puff.sub = subset(mydf_puff, time==time.set[t] & period==period.set[p])
                   puff.response.tmp.sub = subset(puff.response.tmp, time==time.set[t] & period==period.set[p])
                   out = mydf_puff.sub
                   out$power = mydf_puff.sub$power - puff.response.tmp.sub$power
                   out
               }) %>% bind_rows
          }) %>% bind_rows
    return(mydf_puff_subtracted)
}
