source('get_metadata.R')

pi2degree = function(myvector){
    return(myvector/pi * 180)
}

standardizeAngles = function(anglevector){
    return((anglevector+135) %% 270 - 135)
}

mend_jumps = function(mydf){
    limit = which(abs(mydf$dx) > 500)
    for(i in limit){
        mydf$x[i:nrow(mydf)] = mydf$x[i:nrow(mydf)] - mydf$dx[i]
        mydf$dx[i] = 0
    }
    limit = which(abs(mydf$dy) > 250)
    for(i in limit){
        mydf$y[i:nrow(mydf)] = mydf$y[i:nrow(mydf)] - mydf$dy[i]
        mydf$dy[i] = 0
    }
    return(mydf)
}

preprocess = function(filename){
    # read data
    mydf = read.csv(filename, stringsAsFactors=FALSE, header=F)
    colnames(mydf) = c('objectAngularPosition','x','y')
    mydf = addMetaData(mydf, filename)
    
    # convert angle from pi-based to degree-based system
    mydf$objectAngularPosition = pi2degree(mydf$objectAngularPosition)

    # standardize objectAngularPosition
    mydf$objectAngularPosition = standardizeAngles(mydf$objectAngularPosition) 

    # centor x, y
    mydf$x = mydf$x - mydf$x[1]
    mydf$y = mydf$y - mydf$y[1]
    
    # if object appeared on left, flip signs of object angles and x
    if(mydf$objectAngularPosition[1]<0){
        mydf$objectAngularPosition = - mydf$objectAngularPosition
        mydf$x = - mydf$x
    }
    
    # fps control
    finalFrameNo = fps * recordingDuration
    if(nrow(mydf) > finalFrameNo){
        compressionRate = as.integer(nrow(mydf)/finalFrameNo)
        mydf = mydf[seq(1, nrow(mydf), by=compressionRate),]
    }

    # calculate differences
    mydf$dObjectAngularPosition = c(0, diff(mydf$objectAngularPosition))
    mydf$dx = c(0, diff(mydf$x))
    mydf$dy = c(0, diff(mydf$y))

    # translate frame# to sec
    mydf$t = 1:nrow(mydf)/fps
    
    # mend jumps of the mouse pointer as controlled by python
    mydf = mend_jumps(mydf)
    
    # label object's angular velocity (L or R)
    dObjectAngularPosition_label = rep('zero', nrow(mydf))
    dObjectAngularPosition_label[mydf$dObjectAngularPosition>0] = 'R'
    dObjectAngularPosition_label[mydf$dObjectAngularPosition<0] = 'L'
    mydf$dObjectAngularPosition_label = dObjectAngularPosition_label

    # label objectAngularPosition
    objectAngularPosition_label = rep('zero', nrow(mydf))
    objectAngularPosition_label[mydf$objectAngularPosition > 0 & mydf$dObjectAngularPosition != 0] = "R"
    objectAngularPosition_label[mydf$objectAngularPosition < 0 & mydf$dObjectAngularPosition != 0] = "L"
    mydf$objectAngularPosition_label = objectAngularPosition_label
    
    # return
    return(mydf)
}

### calc activity according to the type of analysis ('test' or 'calib')
calcActivity = function(mydf_all){
    activity = with(subset(mydf_all, t < tmax4activity), mean(sqrt(dx^2 + dy^2))*fps) # pixels/s
    return(activity)
}

calcAttractionIndex = function(myvector){
    return(sum(myvector) / sum(abs(myvector)))
#    return(mean(abs(myvector)))
}

### calcs attraction indices for position nad direction per phase per trial, as well as activity index
calcIndices4trials = function(mydf_all){
    # flip sign of dObjectAngularPosition when object on left (2020.10.06)
    mydf_all$dObjectAngularPosition[mydf_all$objectAngularPosition_label=='L'] =
        - with(mydf_all, dObjectAngularPosition[objectAngularPosition_label=='L'])

    # get metadata
    metanames = getMetaNames(mydf_all[1,'filename'])

    # for each trial, calc indices
    result_all = NULL
    for(i in unique(mydf_all$uniq)){
        for(j in unique(mydf_all$trial)){
            for(h in unique(mydf_all$LEDfreq)){
                mydf_subtrial = subset(mydf_all, uniq==i & trial==j & LEDfreq==h & trialtype=='normal')
                if(nrow(mydf_subtrial)==0) next
                
                # summarize
                result = subset(mydf_subtrial,
                                select=intersect(metanames, colnames(mydf_subtrial)))[1,]
                result$activity = calcActivity(mydf_subtrial)
                result$index_onset =
                    with(subset(mydf_subtrial, t>=2 & t<5),
                            calcAttractionIndex(dObjectAngularPosition))
                result$index_final =
                    with(subset(mydf_subtrial, t>=57 & t<60),
                            calcAttractionIndex(dObjectAngularPosition))
                result$index_throughout =
                    with(subset(mydf_subtrial, t>=5 & t<30),
                            calcAttractionIndex(dObjectAngularPosition))
                result_all = rbind(result_all, result)
            }
        }
    }
    return(as.data.frame(result_all))
}

averageIndices4flies = function(result_all){
    # average of different trials of the same fly, for each puff condition
    result_all_mean = NULL
    for(i in unique(result_all$uniq)){
        for(j in unique(result_all$pufffreq)){
            for(k in unique(result_all$LEDfreq)){
                subdata = subset(result_all, uniq==i & pufffreq==j & LEDfreq==k)
                if(nrow(subdata)==0) next
                subdata4average = subdata[,colnames(subdata) %ni% metanames]
                out_meta = subdata[1, intersect(colnames(subdata), metanames[metanames!='trial'])]
                out_mean = lapply(subdata4average, mean)
                out = data.frame(out_meta, out_mean)
                result_all_mean  = rbind(result_all_mean, out)
            }
        }
    }
    result_all_mean = na.omit(as.data.frame(result_all_mean))
    return(result_all_mean)
}

calcPuffeffect = function(result_all_mean){
    # as for attraction index, its already a standardized index -> subtract 
    # as for activity, absolute value -> change (%)
    # set LEDfreq except for 0Hz
    LEDfreq.set = unique(result_all_mean$LEDfreq) %>% sort %>% tail(-1)
    puffeffect = NULL
    for(i in unique(result_all_mean$uniq)){
        for(k in LEDfreq.set){
            subdata = subset(result_all_mean, uniq==i & LEDfreq%in%c(0, k))
            if(length(unique(subdata$LEDfreq))<2) next
            subdata4diff = subset(subdata, select=c(host, genotype, date, LEDfreq, uniq, activity, index_onset, index_final, index_throughout))
			out.0 = subset(subdata4diff, LEDfreq==0, select=c(activity,index_onset,index_final,index_throughout))[1,]
			out.k = subset(subdata4diff, LEDfreq==k, select=c(activity,index_onset,index_final,index_throughout))[1,]
			out = data.frame(subset(subdata4diff, select=c(host, genotype, date, LEDfreq, uniq))[1,], out.k - out.0)
            out$LEDfreq = k

			# convert activity value from absolute difference to percent change
            out$activity = out$activity / out.0$activity * 100
            # add baseline activity
            out$baseactivity = subdata$activity[subdata$LEDfreq==0]
            puffeffect = rbindlist(list(puffeffect, out))
        }
    }
    return(as.data.frame(puffeffect))
}

#######################
# main
#######################
main_batch = function(mypath='../data/ballrecordings/', parallel=TRUE){
    ### assemble all the csv files
    print('assembling ballrecordings...')
    myfiles = list.files(path=mypath, pattern='.csv$')
    myfiles = paste0(mypath, myfiles[grep('GENOTYPE',myfiles)])
    mydf_all = NULL
    cl = makeCluster(numCores, type="FORK")
    registerDoParallel(cl)
    mydf_all = foreach(i=myfiles, .export=ls(.GlobalEnv)) %dopar% {
        preprocess(i)
    }
    stopCluster(cl)
    mydf_all = do.call("rbind", mydf_all)
    trialtype = rep('normal', nrow(mydf_all))
    trialtype[grep('CALIB', mydf_all$filename)] = 'CALIB'
    mydf_all$trialtype = trialtype
    rm(trialtype)
    mydf_all$uniq_trial_trialtype = with(mydf_all, paste(uniq, trial, trialtype, sep='_'))
    assign('mydf_all', mydf_all, envir=.GlobalEnv)
    print('[*] done')
    
    print('calculating indices for each trial...')
    result_all = calcIndices4trials(subset(mydf_all, trialtype=='normal'))
    assign('result_all', result_all, envir = .GlobalEnv)
    print('[*] done')

    print('calculating average of trials for each fly...')
    result_all_mean = averageIndices4flies(result_all)
    assign('result_all_mean', result_all_mean, envir=.GlobalEnv)
    print('[*] done')
    
    print('calculating puff effect...')
    puffeffect = calcPuffeffect(result_all_mean)
    assign('puffeffect', puffeffect, envir=.GlobalEnv)
    print('[*] done')
}
