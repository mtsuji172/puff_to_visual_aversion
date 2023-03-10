library(ggplot2)
library(reshape2)
library(rjson)
library(stringr)
library(data.table)
library(signal)
library(foreach)
library(doParallel)
library(gridExtra)
library(rcompanion)
library(effsize)

fps = 10
recordingDuration = 60
tmax4activity = 5
objectAngularPosition_abs_t0 = 60
activity_thresh = 2 # indices are calculated using frames with activity larger than this thresh

numCores = detectCores() - 2
variable.set = c('mydf_all','result_all','result_all_mean','puffeffect','pose_raw','pose_flymean','pose_flymean_throughout','pose_trialmean','pose_trialmean_throughout','pose_puffeffect','pose_puffeffect_throughout','OKflies','uniq.myset','OKtrials4pose','OKtrials4aversion')

mydf = data.frame(x1=c(-Inf, -Inf), x2=c(Inf, Inf), y1=c(-Inf, 60), y2=c(60,Inf), mycol=c('attraction','aversion'))

import = function(indir){
    for(myvar in variable.set){
        print(paste0(indir, myvar, '.csv'))
        if(file.exists(paste0(indir, myvar, '.csv'))){
            print('[*] found!')
            imported = read.csv(paste0(indir, myvar, '.csv')) 
            if(myvar %in% c('OKflies','uniq.myset','OKtrials4pose','OKtrials4aversion'))
                imported = imported[,1]
            assign(myvar, imported, envir=.GlobalEnv)
        }
    }
}

export = function(outdir){
    dir.create(outdir, showWarnings=F)
    for(myvar in variable.set){
        if(exists(myvar))
            write.csv(eval(parse(text=myvar)), paste0(outdir, myvar, '.csv'), row.names=F)
    }
}
