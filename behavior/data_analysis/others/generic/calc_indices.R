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
    
    # if more than two periods in some values, keep the initial period and turn the rest into zeros
    doublePeriods.pos = mydf$objectAngularPosition %>% as.numeric %>% is.na %>% which
    if(length(doublePeriods.pos)>0){
        print('more than two periods in ballrecordings!: ')
        print(filename)
        doublePeriods = mydf$objectAngularPosition[doublePeriods.pos]
        for(i in 1:length(doublePeriods)){
            myvalue = doublePeriods[i]
            myvalue.split = myvalue %>% strsplit('\\.') %>% unlist
            myvalue.corrected = paste(myvalue.split[1], paste(tail(myvalue.split, -1), collapse='0'), sep='.')
            mydf$objectAngularPosition[doublePeriods.pos[i]] = myvalue.corrected
        }
        mydf$objectAngularPosition = as.numeric(mydf$objectAngularPosition)
    }


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

### calc activity
calcActivity = function(mydf_all){
    activity = with(subset(mydf_all, t < tmax4activity), max(sqrt(dx^2 + dy^2))*fps) # pixels/s
    return(activity)
}

calcAttractionIndex = function(myvector){
    return(sum(myvector) / sum(abs(myvector)))
}

### calcs attraction indices for position nad direction per phase per trial, as well as activity index
calcIndices4trials = function(mydf_all, activity_thresh){
    # flip sign of dObjectAngularPosition when object on left
    mydf_all = mydf_all %>% subset(trialtype=='normal')
    mydf_all$dObjectAngularPosition[mydf_all$objectAngularPosition_label=='L'] =
        - with(mydf_all, dObjectAngularPosition[objectAngularPosition_label=='L'])

    # for each trial, calc indices
    result_all = 
        mydf_all %>% 
        mutate(uniq_trial_npuff_height = paste(uniq, trial, npuff, height, sep='_'), activity=dx^2+dy^2) %>%
        group_by(uniq_trial_npuff_height) %>%
        subset(activity > activity_thresh) %>%
        select(-'activity') %>%
        reframe(genotype=genotype[1], npuff=npuff[1], trial=trial[1], uniq=uniq[1], height=height[1], date=date[1], index_onset = calcAttractionIndex(dObjectAngularPosition[t>=1 & t<3]), index_throughout = calcAttractionIndex(dObjectAngularPosition[t>=1 & t<30]))

    activity = 
        mydf_all %>% 
        subset(t<tmax4activity) %>%
        mutate(uniq_trial_npuff_height = paste(uniq, trial, npuff, height, sep='_')) %>%
        group_by(uniq_trial_npuff_height) %>%
        reframe(activity = max(sqrt(dx^2 + dy^2))*fps)

    return(merge(result_all, activity, by='uniq_trial_npuff_height') %>% select(-'uniq_trial_npuff_height'))
}

averageIndices4flies = function(result_all){
    # average of different trials of the same fly, for each puff condition
    result_all_mean = 
        result_all %>%
        mutate(uniq_npuff_height = paste(uniq, npuff, height, sep='_')) %>%
        group_by(uniq_npuff_height) %>%
        reframe(genotype=genotype[1], npuff=npuff[1], uniq=uniq[1], height=height[1], date=date[1], index_onset = mean(index_onset, na.rm=T), index_throughout = mean(index_throughout, na.rm=T), activity = mean(activity, na.rm=T))
    return(result_all_mean)

}

calcPuffeffect = function(result_all_mean){
    # as for attraction index, its already a standardized index -> subtract 
    # as for activity, absolute value -> change (%)
    # set npuff except for 0Hz
    npuff.set = unique(result_all_mean$npuff) %>% sort %>% tail(-1)
    puffeffect = NULL
    for(i in unique(result_all_mean$uniq)){
        for(k in npuff.set){
            for(h in unique(result_all_mean$height)){
                subdata = subset(result_all_mean, uniq==i & npuff%in%c(0, k) & height==h)
                if(length(unique(subdata$npuff))<2) next
                subdata4diff = subset(subdata, select=c(genotype, date, npuff, height, uniq, activity, index_onset, index_throughout))
                out.0 = subset(subdata4diff, npuff==0, select=c(activity,index_onset,index_throughout))[1,]
                out.k = subset(subdata4diff, npuff==k, select=c(activity,index_onset,index_throughout))[1,]
                out = data.frame(subset(subdata4diff, select=c(genotype, date, npuff, uniq, height))[1,], out.k - out.0)
                out$npuff = k

                # convert activity value from absolute difference to percent change
                out$activity = out$activity / out.0$activity * 100

                # add baseline activity
                out$baseactivity = subdata$activity[subdata$npuff==0]
                puffeffect = rbindlist(list(puffeffect, out))
            }
        }
    }

    return(as.data.frame(puffeffect))
}

#######################
# main
#######################
main_batch = function(mypath='../data/main/ballrecordings/', parallel=TRUE){
    ### assemble all the csv files
    print('assembling ballrecordings...')
    myfiles = list.files(path=mypath, pattern='.csv$')
    myfiles = paste0(mypath, myfiles[grep('GENOTYPE',myfiles)])
    mydf_all = NULL
    if(parallel){
        cl = makeCluster(numCores, type="FORK")
        registerDoParallel(cl)
        mydf_all =
            foreach(i=1:length(myfiles), .export=ls(.GlobalEnv)) %dopar% {
                print(i)
                preprocess(myfiles[i])
            }
        stopCluster(cl)
        mydf_all = do.call("rbind", mydf_all)
    } else{
        mydf_all =
            lapply(1:length(myfiles), function(i){
                       print(i)
                preprocess(myfiles[i])
            })%>% bind_rows
    }
    trialtype = rep('normal', nrow(mydf_all))
    trialtype[grep('CALIB', mydf_all$filename)] = 'CALIB'
    trialtype[grep('ACTIVITY', mydf_all$filename)] = 'ACTIVITY'
    mydf_all$trialtype = trialtype
    rm(trialtype)
    mydf_all$uniq_trial_trialtype = with(mydf_all, paste(uniq, trial, trialtype, sep='_'))
    assign('mydf_all', mydf_all, envir=.GlobalEnv)
    print('[*] done')
    
    print('calculating indices for each trial...')
    result_all = calcIndices4trials(subset(mydf_all, trialtype=='normal' & uniq_trial_trialtype %in% OKtrials4aversion), activity_thresh)
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

