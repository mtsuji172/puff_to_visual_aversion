
gen_dPixels = function(root, overwrite=F){
    # prep dirs
    path2files = paste0(root, 'Pixels/')
    path2save = paste0(root, 'dPixels/')
    dir.create(path2save, showWarnings=F, recursive=T)

    # list files
    myfiles = list.files(path2files, pattern='.csv')

    # if outfile already exists, skip
    if(!overwrite){
        myfiles.select = 
            lapply(1:length(myfiles), function(f){
                if(!file.exists(paste0(path2save, myfiles[f]))){
                    myfiles[f]
                }
            }) %>% c %>% unlist
        myfiles = myfiles.select
        rm(myfiles.select)
    }

    # if no file to process, exit
    stopifnot(length(myfiles) > 0)

    # loop over files in parallel
    cl = makeCluster(ncores)
    registerDoParallel(cl)
    foreach(f=1:length(myfiles),
                        .combine=rbind,
                        .export = ls(globalenv()),
                        .packages=c('dplyr', 'reshape2')) %dopar%{
        # read file
        myfile = myfiles[f]
        mydata = read.csv(paste0(path2files, myfile), header=T)
        mydata = mydata[,-1]

        # calculate dFrames
        mydata.diff = apply(mydata, 2, diff) %>% as.data.frame

        # save
        write.csv(mydata.diff, paste0(path2save, myfiles[f]), row.names=F)
    }
    stopCluster(cl)
}

adjustNchar = function(myint, mylength){
    Nchar = nchar(as.character(myint))
    Nchar.diff = mylength - Nchar
    if(Nchar.diff > 0){
        myint.new = rep('0',Nchar.diff) %>% {paste(., collapse='')} %>% {paste0(., as.character(myint))}
        
    } else{
        myint.new = myint
    }
    return(myint.new)
}

plotframe = function(myframe, outdir, k){
    # threshold
    myframe.vec = as.vector(as.matrix(myframe))

    # convert to matrix
    myframe.mat = matrix(myframe.vec, nrow=xdim) %>% t

    # plot
    png(paste0(outdir, adjustNchar(k, 3), '.png'))
    gplots::heatmap.2(myframe.mat, Rowv=FALSE, Colv=FALSE, dendrogram="none", xlab="Columns", ylab="Rows", col='bluered', tracecol="#303030", trace="none", keysize = 1.5, margins=c(5, 5))
    dev.off()
}

plotframes = function(root, overwrite=F){
    # prep dirs
    path2files = paste0(root, 'dPixels/')
    path2save = paste0(root, 'dPixels_image/')
    dir.create(path2save, showWarnings=F, recursive=T)

    # list files (only trials 0 and 1)
    myfiles = list.files(path2files, pattern='.csv')
    if(any(grep('TRIAL2',myfiles))){
        myfiles = myfiles[-grep('TRIAL2',myfiles)]
    }

    # if outfile already exists, skip
    if(!overwrite){
        myfiles.select = 
            lapply(1:length(myfiles), function(f){
                if(!file.exists(paste0(path2save, myfiles[f]))){
                    myfiles[f]
                }
             }) %>% c %>% unlist
        myfiles = myfiles.select
        rm(myfiles.select)
    }
    print(myfiles)

    # loop over files
    lapply(1:length(myfiles), function(f){    

        # read file
        mydata = read.csv(paste0(path2files, myfiles[f]), header=T)

        # prep outdir
        path2save.f = paste0(path2save, myfiles[f],'/') %>% {gsub('.csv','',.)}
        dir.create(path2save.f, showWarnings=F, recursive=T)

        # save frames as images
        lapply(1:nrow(mydata), function(k){
            print(k)
            plotframe(mydata[k,], outdir=path2save.f, k)
        })
    })
}

get_yDistance = function(colname_array){
    # translate colnames into original y positions
    x = colname_array %% xdim
    y = colname_array %/% xdim
    
    # get the variance of y coordinates
    distance = sqrt(var(y))

    # return
    return(distance)
}

get_yDistance_row = function(myframe){

    # calculate average y distances of +pixels
    pixels.posi = which(myframe > 0) - 1
    pixels.posi.distance = get_yDistance(pixels.posi)

    # calculate average y distances of -pixels
    pixels.nega = which(myframe < 0) - 1
    pixels.nega.distance = get_yDistance(pixels.nega)

    # posi - nega
    yDistance = pixels.posi.distance - pixels.nega.distance
    return(yDistance)
}

get_peaks = function(myvector){
    peaks = 
        lapply(1:(length(myvector)-1), function(k){
            ifelse((myvector[k] < 0) & (myvector[k+1]>0), 1, 0)
         }) %>% c %>% unlist
    peaks = c(peaks, 0)

    return(peaks)
}


detectBeats = function(root, overwrite=F){
    # prep dirs
    path2files = paste0(root, 'dPixels/')
    path2save = paste0(root, 'dPixels_distance/')
    dir.create(path2save, showWarnings=F, recursive=T)

    # if outfile already exists, skip
    if(!overwrite){
        myfiles.select = 
            lapply(1:length(myfiles), function(f){
                if(!file.exists(paste0(path2save, myfiles[f]))){
                    myfiles[f]
                }
             }) %>% c %>% unlist
        myfiles = myfiles.select
        rm(myfiles.select)
    }
    print(myfiles)

    # loop over files in parallel
    cl = makeCluster(ncores)
    registerDoParallel(cl)
    foreach(f=1:length(myfiles),
                        .combine=c,
                        .export = ls(globalenv()),
                        .packages=c('dplyr', 'reshape2')) %dopar%{
        # get metadata
        metadata_nopath = myfiles[f] %>% {gsub('.csv','.json',.)}
        metadata = rjson::fromJSON(file=paste0(root, 'metadata/', metadata_nopath)) %>% as.data.frame
        metadata$uniq_trial = with(metadata, paste(DATE, ID, TRIAL, sep='_'))
        metadata$uniq_fly = with(metadata, paste(DATE, ID, sep='_'))
        
        # read file
        mydata = read.csv(paste0(path2files, myfiles[f]), header=T)

        # calculate the change in typical y distance between current and previous frames
        yDistance.diff = apply(mydata, 1, get_yDistance_row)

        # if yDistance.diff contains NA (can happpen when the frame momentarily becomes super dark and thus the total number of pixels.posi or pixels.nega can become smaller than 2, which then makes it impossible to calculate SD of y coordinates.)
        if(any(is.na(yDistance.diff))){
            NApos = which(is.na(yDistance.diff))
            yDistance.diff = zoo::na.approx(yDistance.diff)
            # if NA in head or tail, na.approx just deletes it -> fill with NA again
            if(1 %in% NApos){
                yDistance.diff = c(NA, yDistance.diff)
            }
            if(length(yDistance.diff) %in% NApos){
                yDistance.diff = c(yDistance.diff, NA)
            }
        }

        # beat detection
        contraction.peak = get_peaks(yDistance.diff)

        # save
        out = cbind(frame = 2:(length(yDistance.diff)+1), 
                    time = trialduration / (metadata$fnum-2) * (0:(metadata$fnum-3)), # the last frame of each video is removed, as the last frame is often only a partial image
                    metadata, 
                    yDistance.diff,
                    contraction.peak
                )
        write.csv(out, paste0(path2save, myfiles[f]), row.names=F)
    }
    stopCluster(cl)
}

get_timecourse = function(mydata.dist, zscore=T){

    # bin time (1s bin)
    mydata.dist$time = mydata.dist$time %/% 1 + 1

    # calc HR
    uniq_fly.set = unique(mydata.dist$uniq_fly)
    cl = makeCluster(ncores)
    registerDoParallel(cl)
    mydata.HR =
        foreach(f=1:length(uniq_fly.set),
                .combine=rbind,
                .export = ls(globalenv()),
                .packages=c('dplyr', 'reshape2')) %dopar%{
            subdata.fly = subset(mydata.dist, uniq_fly == uniq_fly.set[f])
            ntrials = length(unique(subdata.fly$uniq_trial))
            time.set = unique(subdata.fly$time)
            out.fly = 
                lapply(1:length(time.set), function(k){
                    subdata = subset(subdata.fly, time==time.set[k])
                    out = subset(subdata, select=-c(contraction.peak))[1,]
                    out$HR = with(subdata, sum(contraction.peak==1) / ntrials)
                    out
                }) %>% bind_rows

            # HR into zscore
            if(zscore)
                out.fly$HR = with(out.fly, (HR - mean(HR)) / sqrt(var(HR)))
            
            # return
            out.fly
         }

    return(mydata.HR)
}

get_windowave = function(mydata.dist, timewindow){

    uniq_fly.set = unique(mydata.dist$uniq_fly)
    mydata.dist.windowave =
        lapply(1:length(uniq_fly.set), function(u){
            
           # get data of a fly
           subdata = subset(mydata.dist, uniq_fly==uniq_fly.set[u] & time >= timewindow[1] & time < timewindow[2])
           ntrials = length(unique(subdata$TRIAL))
           out = select(subdata, -any_of(c('fnum', 'time', 'contraction.peak', 'ID', 'TRIAL', 'frame')))[1,]
           out$HR = sum(subdata$contraction.peak==1) / diff(timewindow) / ntrials
           out
        }) %>% bind_rows
    return(mydata.dist.windowave)
}
