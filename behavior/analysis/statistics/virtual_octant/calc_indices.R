source('get_metadata.R')
library(dplyr)

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
    colnames(mydf) = c('puff','angle','x','y')
    
    # determine octant arrangement ('right' or 'reverse') -> flip sign of angle if 'reverse'
    if(mydf$puff[which(mydf$angle>0)%>%head(1)] == 'ON'){
        arrangement = 'right'
    } else{
        arrangement = 'reverse'
        mydf$angle = -mydf$angle
    }
    mydf$arrangement = arrangement

    # add metadata
    mydf = addMetaData(mydf, filename)

    # convert angle from pi-based to degree-based system
    mydf$angle = pi2degree(mydf$angle)

    # standardize angle
    mydf$angle = standardizeAngles(mydf$angle) 

    # center x, y
    mydf$x = mydf$x - mydf$x[1]
    mydf$y = mydf$y - mydf$y[1]
    
    # calculate differences
    mydf$dangle = c(0, diff(mydf$angle))
    mydf$dx = c(0, diff(mydf$x))
    mydf$dy = c(0, diff(mydf$y))

    # translate frame# to sec
    fps = nrow(mydf) / recordingDuration
    mydf$t = 0:(nrow(mydf)-1)/fps
    
    # mend jumps of the mouse pointer as controlled by python
    mydf = mend_jumps(mydf)
    
    # return
    return(mydf)
}

calcAttractionIndex = function(myvector){
    return(sum(myvector=='ON') / length(myvector))
}

MovementAroundEntry = function(mydf){
    LED = mydf$LED
    nonBoutFrame = which(LED == 'OFF')
    dyBeforeEntry = NA
    dyAfterEntry = NA
    absdxBeforeEntry = NA
    absdxAfterEntry = NA

    # exception handling: no change in ON/OFF -> end function 
    if(length(nonBoutFrame) %in% c(0, 1, length(LED))){
        return(list(dyBeforeEntry, dyAfterEntry, absdxBeforeEntry, absdxAfterEntry))
    }
    
    # loop over nonBoutFrame
    for(i in 1:(length(nonBoutFrame)-1)){
        boutlength  = ((nonBoutFrame[i+1] - nonBoutFrame[i]) - 1 ) / fps # in sec
        if(boutlength > 0 & boutlength < 3){
            dyBeforeEntry = c(dyBeforeEntry, mean(mydf$dy[max(0, nonBoutFrame[i] - fps):nonBoutFrame[i]]))
            dyAfterEntry = c(dyAfterEntry, mean(mydf$dy[nonBoutFrame[i]:min((nonBoutFrame[i]+fps),nrow(mydf))]))
            absdxBeforeEntry = c(absdxBeforeEntry, mean(abs(mydf$dx[max(0, nonBoutFrame[i] - fps):nonBoutFrame[i]])))
            absdxAfterEntry = c(absdxAfterEntry, mean(abs(mydf$dx[nonBoutFrame[i]:min((nonBoutFrame[i]+fps),nrow(mydf))])))
        }
    }
    return(list((dyBeforeEntry), (dyAfterEntry), (absdxBeforeEntry), (absdxAfterEntry)))
}

countBout = function(mydf, boutType){
    # boutType = 'exit' or 'entry'

    # get fps
    fps = nrow(mydf) / recordingDuration

    actionCategory = rep('still', nrow(mydf))
    nPassThrough = 0 
    nTurnAround = 0
    puff = mydf$puff
    nonBoutFrame = which(puff == ifelse(boutType=='exit', 'OFF', 'ON'))
    
    # exception handling: no change in ON/OFF -> end function 
    if(length(nonBoutFrame) %in% c(0, 1, length(puff))){
        return(list(actionCategory, nPassThrough, nTurnAround))
    }
    
    # exception handling: 1st frame is boutType 
    if(puff[1]==ifelse(boutType=='exit', 'ON', 'OFF')){
        timeExit = nonBoutFrame[1]
        boutlength = timeExit / fps
        EntryExitAngleDiff = abs(mydf[1,'angle'] - mydf[timeExit, 'angle'])
        if(boutlength < 3 & EntryExitAngleDiff>=10){
            actionCategory[1:(timeExit-1)] = paste0('rushThrough_', boutType)
            nPassThrough = nPassThrough + 1
        }
    }
    
    # loop over nonBoutFrame
    for(i in 1:(length(nonBoutFrame)-1)){
        boutlength  = ((nonBoutFrame[i+1] - nonBoutFrame[i]) - 1 ) / fps # in sec
        if(boutlength > 0 & boutlength < 3){
            EntryExitAngleDiff = abs(mydf[nonBoutFrame[i],'angle'] - mydf[nonBoutFrame[i+1], 'angle'])
            # check if bout i is pass-through
            if(EntryExitAngleDiff>=10){
                actionCategory[nonBoutFrame[i]:(nonBoutFrame[i+1]-1)] = paste0('rushThrough_', boutType)
                nPassThrough = nPassThrough + 1 
            } else{
                # check if bout i is turn-around
                pastfuture1s = max((nonBoutFrame[i]-fps),1):min((nonBoutFrame[i]+fps),nrow(mydf))
                distanceTravelledPastFuture1s = sum(sqrt(mydf$dx[pastfuture1s]^2 + mydf$dy[pastfuture1s]^2))
                if(distanceTravelledPastFuture1s > 400){
                    actionCategory[nonBoutFrame[i]:(nonBoutFrame[i+1]-1)] = paste0('turnAround_', boutType)
                    nTurnAround = nTurnAround + 1
                }
            }
        }
    }
    
    return(list(actionCategory, nPassThrough, nTurnAround))
}

calcIndices = function(mydf_all){

    # for each trial, calc indices
    uniq.set = unique(mydf_all$uniq)
    #result_all = foreach(i=1:length(uniq.set), .export=ls(.GlobalEnv), .combine=rbind) %dopar% {
    result_all = lapply(1:length(uniq.set), function(i){
                            print(i)

        # get data
        mydf = subset(mydf_all, uniq==uniq.set[i])
        out = mydf[1, c('genotype','host','date','uniq')]

        # calc attraction index (relative time spent in "puff-ON" angles
        out$attractionIndex = calcAttractionIndex(mydf$puff)

        # count exit bouts 
        countBout_out = countBout(mydf, 'exit')
        mydf$actionCategory = as.factor(countBout_out[[1]])
        out$nPassThrough_exit = countBout_out[[2]]
        out$nTurnAround_exit = countBout_out[[3]]

        # count entry bouts
        countBout_out = countBout(mydf, 'entry')
        out$nPassThrough_entry = countBout_out[[2]]
        out$nTurnAround_entry = countBout_out[[3]]

        # out
        as.data.frame(out)
    }) %>% bind_rows
    return(result_all)
}
