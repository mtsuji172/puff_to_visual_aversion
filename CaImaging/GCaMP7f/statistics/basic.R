get_index = function(vect1, vect2){
    return((vect1-vect2))
}

# get last n char from string
substrTail = function(x, n){
	substr(x, nchar(x)-n+1, nchar(x))
}

# select subset of mydata based on keywords in column "ID"
selectData = function(mydata, keyT=NULL, keyF=NULL){
    mydata.sub = mydata
    if(!is.null(keyT)){
        for(mykeyT in keyT){
            if(any(grep(mykeyT, mydata.sub$ID))){
                mydata.sub = mydata.sub[grep(mykeyT, mydata.sub$ID),]
            }
        }
    }
    if(!is.null(keyF)){
        for(mykeyF in keyF){
            if(any(grep(mykeyF, mydata.sub$ID))){
                mydata.sub = mydata.sub[-grep(mykeyF, mydata.sub$ID),]
            }
        }
    }
    return(mydata.sub)
}

findmax = function(x){
    return(which(x==max(x))[1])
}

batchcalc = function(mypath){
    # list dirs
    mydirs = paste0(list.dirs(path=mypath, recursive=FALSE), '/GminusR/aligned/')
    mydirs = mydirs[grep('ID', mydirs)]
    mydirs = gsub('//','/', mydirs)

    # load result
    print('loading results...')
    counter.all = length(mydirs)
    counter = 1
    for(mydir in mydirs){
        print("########################")
        print(paste0("processing ", mydir, " : ", counter, "/", counter.all))
        print("########################")

        ### normalize and add info
        outname_base = paste0(mypath, mydir)
        mydata = try(read.csv(paste0(mydir,'/Results.csv')), silent=F)
        if (file.exists(paste0(outname_base, 'mydata.csv'))){
            counter = counter + 1
            next
        }
        if (class(mydata) == "try-error"){
            counter = counter + 1
            next
        }
        
        # normalize the time
        mydata$X = ((mydata$X-1) * sessionDuration / max(mydata$X-1)) %>% round(3)
        
        # select signal columns
        mydata = mydata[,c(1,grep('Mean',colnames(mydata)))]

        # melt
        mydata = melt(mydata, id.vars='X', variable.name='ROI', value.name='intensity')
        
        # detrend
        linear_fit = lm(intensity~X, data=mydata)
        mydata$intensity = mydata$intensity - linear_fit$fitted.values + linear_fit$fitted.values[1]
    
        # add info
        mydata$ID = tail(unlist(strsplit(mydir,'/')),3)[1]
        mydata$ROI = gsub('Mean','',mydata$ROI)
        mydata$ID_ROI = with(mydata, paste(ID, ROI, sep='_'))

        # normalize the value for each ROI
        print('transforming intensities into z-score and deltaF...')
        mydata.normalized = NULL
        for(k in unique(mydata$ID_ROI)){
            subdata = subset(mydata, ID_ROI==k)
            subdata$zscore = with(subdata, (intensity - mean(intensity)) / sqrt(var(intensity)))
            subdata$dF = with(subdata, ((intensity - mean(intensity[X>(ONtime[1]-acceptedrange) & X<=ONtime[1]])) / mean(intensity[X>(ONtime[1]-acceptedrange) & X<=ONtime[1]]))*100)
            mydata.normalized = rbind(mydata.normalized, subset(subdata, select=-intensity))
        }
        mydata = mydata.normalized; rm(mydata.normalized)
        mydata$X = as.numeric(as.character(mydata$X))

        # export
        write.csv(mydata, paste0(outname_base, 'mydata.csv'), row.names=F)
        print('done')

        ### bin time
        print('binning mydata...')
        mydata.bin = mydata
        mydata.bin$X = cut(mydata$X, 
                         breaks=seq(min(mydata$X), max(mydata$X)+targetdt, by=targetdt), 
                         include.lowest=T, 
                         rgiht=F, 
                         labels=seq(min(mydata$X), max(mydata$X), by=targetdt))
        mydata.bin$uniq = with(mydata.bin, paste(ID_ROI, X, sep='_'))
        uniq.set = unique(mydata.bin$uniq)
        cl = makeCluster(ncore)
        registerDoParallel(cl)
        mydata.bin.mean = foreach(k=1:length(uniq.set),
                                .combine=rbind,
                                .export = ls(globalenv()),
                                .packages=c('dplyr')) %dopar%{
            subdata = subset(mydata.bin, uniq==uniq.set[k])
            out = subdata[1,]
            out$zscore = mean(subdata$zscore)
            out$dF = mean(subdata$dF)
            out
        }
        stopCluster(cl)
        mydata.bin = mydata.bin.mean
        rm(mydata.bin.mean)
        mydata.bin$X = mydata.bin$X %>% as.character %>% as.numeric

        # export
        write.csv(mydata.bin, paste0(outname_base, 'mydata.bin.csv'), row.names=F)
        print('done')
        
        ### calc mean / puffcondition / fly_ROI
        print('calculating mean per puff condition per ROI...')
        ID_ROI.set = unique(mydata.bin$ID_ROI)
        mydata.flyROImean = lapply(1:length(ID_ROI.set), function(h){
            subdata.h = subset(mydata.bin, ID_ROI == ID_ROI.set[h])
            subdata.h$phase = 'base'
            for(k in 1:length(myschedule)){
                for(i in 1:length(myschedule[[k]])){
                    subdata.h$phase[with(subdata.h, X>=myschedule[[k]][[i]][1] & X<myschedule[[k]][[i]][2])] = names(myschedule)[k]
                }
            }
            phase.set = names(myschedule) %>% unique
            out = lapply(1:length(phase.set), function(myphase){
                    subdata.h.myphase = subset(subdata.h, phase==phase.set[myphase]) 
                    out.h.myphase =
                        data.frame(ID_ROI=ID_ROI.set[h],
                                   phase=phase.set[myphase],
                                   zscore = mean(subdata.h.myphase$zscore),
                                   dF = mean(subdata.h.myphase$dF))
                     out.h.myphase
                 }) %>% bind_rows
            out
        }) %>% bind_rows

        # export
        write.csv(mydata.flyROImean, paste0(outname_base, 'mydata.flyROImean.csv'), row.names=F)
        print('done')
        
        ### calc puffeffect for each fly_ROI
        print('calculating puff effect per ROI...')
        puffeffect = lapply(1:length(ID_ROI.set), function(h){
            subdata = subset(mydata.flyROImean, ID_ROI == ID_ROI.set[h])
            subdata$zscore = with(subdata, zscore - zscore[phase=='base'])
            subdata$dF = with(subdata, dF - dF[phase=='base'])
            subset(subdata, phase!='base')
        }) %>% bind_rows

        # export
        write.csv(puffeffect, paste0(outname_base, 'puffeffect.csv'), row.names=F)
        print('done')

        ### update counter
        counter = counter + 1
    }
}

mergeVar = function(dfname, suffices, outsuffix='_merge'){
    dfname1 = paste0(dfname, suffices[1])
    dfname2 = paste0(dfname, suffices[2])
    if(exists(dfname1) & exists(dfname2)){
        print('OK')
        df1 = eval(parse(text=dfname1))
        df2 = eval(parse(text=dfname2))
        colset = intersect(colnames(df1), colnames(df2))
        out = rbind(subset(df1,select=colset),subset(df2,select=colset))
    } else{
        if(exists(dfname1)){
            df1 = eval(parse(text=dfname1))
            out = df1
        } else{
            if(exists(dfname1)){
                df2 = eval(parse(text=dfname2))
                out = df2
            } else{
                stop("neither object exists! stopping...")
            }
        }
    }
    assign(paste0(dfname, outsuffix), out, envir=.GlobalEnv)
}

exportdata = function(outdir){
    dir.create(paste0(outdir,'/export'), showWarnings=F)
    for(myvar in varnames.set){
        tryCatch(
                 {write.csv(eval(parse(text=myvar)), paste0(outdir,'/export/',myvar,'.csv'), row.names=F)}, 
                 error=function(e){print(paste0(myvar, ': no such variable. skipping')); NA}
                 )
        
    }
}

importdata = function(indir){
    for(myvar in varnames.set){
        tryCatch({assign(myvar, read.csv(paste0(indir,'/',myvar,'.csv'), header=T, stringsAsFactors=F), envir=.GlobalEnv)}, error=function(e){print(paste0(myvar, ': no such variable. skipping')); NA})
    }
}

readfiles = function(indir, mypattern='.csv$'){
    myfiles = list.files(indir, pattern=mypattern)
    out = lapply(1:length(myfiles), function(k){
                     myfile = read.csv(paste0(indir, '/', myfiles[k]))
                     myfile$file = myfiles[k]
                     myfile
                 }) %>% bind_rows
    return(out)
}
