library(ggplot2)
library(stringr)
library(reshape2)
library(gridExtra)
library(rjson)
library(progress)
library(compiler)
library(DescTools)
library(dplyr)

# params
labels = c('flight','walk','stop','grooming','PER','freezeInAir','stuck')
nlabel = length(labels)
ethogram_colors <<- c('flight'='yellow1','walk'='red','grooming'='deepskyblue1','grooming_thorax'='magenta','PER'='blue','stop'='gray70','freezeInAir'='black')
ethogram_colors4smooth <<- c('flight'='gold3','walk'='red','grooming'='deepskyblue1','grooming_thorax'='magenta','PER'='blue','stop'='gray70','freezeInAir'='black')

### functions
extractInfo = function(pose_raw, keyword){
    mysplit = str_split_fixed(as.character(pose_raw[,2]), '_', 6)
    if(keyword=='uniq'){
        extractedInfo = apply(mysplit[,c(1,2,3)], 1, paste, collapse='_')
    } else{
        if(keyword=='frame'){
            extractedInfo = as.numeric(gsub('ACTIVITY_','',gsub('CALIB_','',gsub('.png','',gsub('.jpg','',mysplit[,6])))))
        } else{
                extractedInfo = gsub('.avi','',gsub(keyword, '', mysplit[grep(keyword,mysplit)]))
        }
    }
    return(extractedInfo)
}

# check the labelling coverage (ratio of frames labelled successfully)
label_coverage = function(pose_raw){
    na.num = sum(is.na(pose_raw$label))
    coverage = (nrow(pose_raw) - na.num) / nrow(pose_raw)
    return(coverage)
    #print(paste0('coverage of the labelling was: ', as.character(coverage)))
}

calc_flymean = function(pose_trialmean){
    uniq.set = unique(pose_trialmean$uniq)
    trialtype.set = unique(pose_trialmean$trialtype)
    npuff.set = unique(pose_trialmean$npuff)
	pose_flymean =  
        lapply(1:length(uniq.set), function(u){
            lapply(1:length(trialtype.set), function(t){
                lapply(1:length(npuff.set), function(n){
                    subdata = subset(pose_trialmean, uniq==uniq.set[u] & trialtype==trialtype.set[t] & npuff==npuff.set[n])
                    if(nrow(subdata)>0){
                        myratio = lapply(subdata[,(ncol(subdata)-nlabel+1):ncol(subdata)], mean) %>% as.data.frame
                        cbind(subdata[1,c('genotype','npuff','uniq','trialtype','postPuffInterval','host')], myratio)
                    }
                }) %>% bind_rows
            }) %>% bind_rows
        }) %>% bind_rows
	pose_flymean = reshape2::melt(pose_flymean, id.vars=colnames(pose_flymean)[1:(ncol(pose_flymean)-nlabel)], variable.name='label', value.name='ratio')
	return(pose_flymean)
}

readfiles = function(mypath, mypattern){
    myfiles = list.files(path=mypath, pattern=mypattern)
    alldata = lapply(1:length(myfiles), function(k){
                    myfile = read.csv(paste0(mypath,myfiles[k]), header=T)
                    myfile
               }) %>% bind_rows()
    return(alldata)
}

selectOKtrials4pose = function(pose_trialmean){
	for(mycol in c('freezeInAir','stuck')){
		if(any(grep(mycol, colnames(pose_trialmean)))){
			pose_trialmean = pose_trialmean[pose_trialmean[,mycol]<0.1, ]
		}
	}
    return(pose_trialmean$uniq_trial_trialtype) 
}

selectOKtrials4aversion = function(pose_trialmean){
	for(mycol in c('freezeInAir','stuck', 'flight')){
		if(any(grep(mycol, colnames(pose_trialmean)))){
			pose_trialmean = pose_trialmean[pose_trialmean[,mycol]<0.1, ]
		}
	}
    return(pose_trialmean$uniq_trial_trialtype) 
}

ethogram = function(pose_raw){
    myplot = 
        ggplot(pose_raw, aes(t, uniq_trial_trialtype, fill=label)) +
           theme_bw() +
           theme(axis.text.y=element_blank(), text=element_text(size=20), legend.position='None') +
           geom_tile() +
           labs(y='trials', x='time (s)', title=unique(pose_raw$npuff)) +
           scale_fill_manual(values=ethogram_colors)
       return(myplot)
}

statPosePuffEffect.inGeno = function(pose_puffeffect, p.adjust.method){
    out = NULL
    for(i in unique(pose_puffeffect$label)){
        for(g in unique(pose_puffeffect$genotype)){
            subpose_puffeffect = subset(pose_puffeffect, label==i & genotype==g)
            if(nrow(subpose_puffeffect)>0){
                shapiro = subpose_puffeffect$ratio %>% shapiro.test
                print(shapiro) %>% capture.output %>% write.table(paste0(mydir, '/pose',i,'_genotype',g,'_shapiro_stat.txt'), row.names=F, col.names=F)
                if(shapiro$p.value < 0.05){
                    mytest = wilcox.test(subpose_puffeffect$ratio)
                    effsize = wilcoxonOneSampleRC(subpose_puffeffect$ratio, mu=0)
                } else{
                    mytest = t.test(subpose_puffeffect$ratio)
                    effsize = with(subpose_puffeffect, mean(ratio)/sd(ratio))
                }
                mytest[sapply(mytest, is.null)] = NA
                mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
                colnames(mytest.df) = c('method','parameter','statistic','p.value')
                out.sub = data.frame(genotype=g, label=i, mytest.df, effsize)
                out = rbind(out, out.sub)
            } else{
                print(paste0("error in label: ", i, " genotype: ", g))
            }
        }
    }
    out$p.adjusted = p.adjust(out$p.value, method=p.adjust.method)
    return(out)
}

statPosePuffEffect.interGeno = function(pose_puffeffect, control, p.adjust.method){
    pose_puffeffect$genotype = as.character(pose_puffeffect$genotype)
    label.set = unique(pose_puffeffect$label)
    genotype.set = unique(pose_puffeffect$genotype)
    genotype.set = genotype.set[genotype.set!=control]
    subdata = subset(pose_puffeffect, label%in%c('walk','stop','grooming'))
    out = NULL
    for(l in 1:length(label.set)){
        subdata.l = subset(subdata, label==label.set[l])
        print(label.set[l])
        print(summary(aov(ratio~genotype, subdata.l)))
        for(g in 1:length(genotype.set)){
            subdata.g = subset(subdata.l, genotype%in%c(control, genotype.set[g]))
            shapiro_ctrl = shapiro.test(subdata.g$ratio[subdata.g$genotype==control])$p.value
            shapiro_test = shapiro.test(subdata.g$ratio[subdata.g$genotype==genotype.set[g]])$p.value

            if(shapiro_ctrl >= 0.05 & shapiro_test>= 0.05){
                mytest = t.test(ratio ~ genotype, data = subdata.g)
                effsize = cohen.d(ratio ~ genotype, data = subdata.g)$estimate
            } else{
                mytest = wilcox.test(ratio ~ genotype, data = subdata.g)
                effsize = with(subdata.g, wilcoxonRG(ratio, genotype))
            }
            mytest[sapply(mytest, is.null)] = NA
            mytest.df = with(mytest, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t
            colnames(mytest.df) = c('method','parameter','statistic','p.value')
            out = rbind(out, data.frame(genotype=genotype.set[g], label=label.set[l], mytest.df, effsize))
        }
    }
    out$p.adjusted = p.adjust(out$p.value, method=p.adjust.method)
    return(out)
}

calc_stopeffect = function(pose_flymean){
    pose_flymean = subset(pose_flymean, trialtype!='CALIB' & npuff==0)
    out = NULL
    for(i in unique(pose_flymean$uniq)){
        for(k in labels){
            subout = NULL
            subdata = subset(pose_flymean, uniq==i & label==k, select=-npuff)
            if(nrow(subdata)!=3){
                print(paste0(i,k,' skipped!:'))
                print(subdata)
                next
            }
            subout = subset(subdata, select=-ratio)[1,]
            mystatistic = with(subdata, 
                               ratio[trialtype=='normal']-ratio[trialtype=='ACTIVI'],
                               )
            subout$ratio = mystatistic
            out = rbind(out, subout)
        }
    }
    return(out)
}

######################################
# main
######################################

loadData = function(predpath, statpath){
    print('loading pose_raw...')
    pose_raw = readfiles(mypath=predpath, mypattern='_labeled.csv$')
    pose_raw$label = pose_raw$label %>% {gsub("\\[","",.)} %>% {gsub("\\]","",.)} %>% {gsub("'","",.)}
    pose_raw$t = pose_raw$frame / 583 * 60
    pose_raw = subset(pose_raw, select=-X)
    assign('pose_raw', pose_raw, envir=.GlobalEnv)
    print('[*] success')

    ######################################
    # throughout the recording
    ######################################
    if(dir.exists(paste0(statpath,'/trialmean_throughout/'))){
        # load trial mean for select time frames (done by python)
        print('loading pose_trialmean_throughout...')
        pose_trialmean = readfiles(mypath=paste0(statpath,'/trialmean_throughout/'), mypattern='.csv$')
        pose_trialmean$label = gsub('NA.s','NA',pose_trialmean$label)
        pose_trialmean = reshape2::dcast(pose_trialmean, genotype+trial+uniq+npuff+postPuffInterval+trialtype+uniq_trial_trialtype+host~label, value.var='ratio')
        assign('pose_trialmean_throughout', pose_trialmean, envir=.GlobalEnv)
        print('[*] success')

        # calc fly mean from trial mean
        print('calculating fly mean...')
        OKtrials4pose = selectOKtrials4pose(pose_trialmean)
        OKtrials4aversion = selectOKtrials4aversion(pose_trialmean)
        uniq_trial_trialtype = with(pose_trialmean, paste(uniq, trial, trialtype, sep='_'))
        pose_flymean = calc_flymean(pose_trialmean[uniq_trial_trialtype%in%OKtrials4pose,])
        assign('OKtrials4pose_throughout',OKtrials4pose, envir=.GlobalEnv)
        assign('OKtrials4aversion_throughout',OKtrials4aversion, envir=.GlobalEnv)
        assign('pose_flymean_throughout', pose_flymean, envir=.GlobalEnv)
       
        # calc puff effect from fly mean
        print('calculating puff effect...')
        # remove CALIB
        pose_flymean = subset(pose_flymean, trialtype=='normal')
        uniq.myset = pose_flymean$uniq %>% unique
        label.set = unique(pose_flymean$label)
        pose_puffeffect = 
            lapply(1:length(uniq.myset), function(k){
               subdata = subset(pose_flymean, uniq==uniq.myset[k])
                out =
                     if(nrow(subdata)>0){
                         subdata.dcast = reshape2::dcast(subdata, genotype+npuff+uniq+trialtype+postPuffInterval+host~label, value.var='ratio')
                         if(nrow(subdata.dcast)>1 & any(subdata.dcast$npuff==0)){
                             out.h = subset(subdata.dcast,
                                          select=c(genotype, npuff, uniq, trialtype, postPuffInterval, host))[1:length(unique(subdata.dcast$npuff)),]
                             tmp = subset(subdata.dcast, select=-c(genotype, npuff, uniq, trialtype, postPuffInterval, host))
                             out.h.tmp = lapply(1:ncol(tmp), function(m){
                                        tmp[,m] - tmp[subdata.dcast$npuff==0, m]
                                          }) %>% bind_cols
                             colnames(out.h.tmp) = colnames(tmp)
                             out.h.tmp = out.h.tmp %>% as.list %>% as.data.frame
                             cbind(out.h, out.h.tmp) %>%
                             {subset(., npuff!=0)}
                         }
                     }
                }) %>% bind_rows
        pose_puffeffect.melt = reshape2::melt(pose_puffeffect, id.vars=c('genotype','npuff','uniq','trialtype','postPuffInterval', 'host'), value.name='ratio', variable.name='label')
        assign('pose_puffeffect_throughout', pose_puffeffect.melt, envir=.GlobalEnv)
    }
    
    ##################################
    # initial 5s
    ##################################
    # load trial mean for select time frames (done by python)
    print('loading pose_trialmean...')
    pose_trialmean = readfiles(mypath=paste0(statpath,'/trialmean/'), mypattern='.csv$')
    pose_trialmean$label = gsub('NA.s','NA',pose_trialmean$label)
    pose_trialmean = reshape2::dcast(pose_trialmean, genotype+trial+uniq+npuff+postPuffInterval+trialtype+uniq_trial_trialtype+host~label, value.var='ratio')
    assign('pose_trialmean', pose_trialmean, envir=.GlobalEnv)
    print('[*] success')

    # calc fly mean from trial mean
    print('calculating fly mean...')
    OKtrials4pose = selectOKtrials4pose(pose_trialmean)
    OKtrials4aversion = selectOKtrials4aversion(pose_trialmean)
    uniq_trial_trialtype = with(pose_trialmean, paste(uniq, trial, trialtype, sep='_'))
    pose_flymean = calc_flymean(pose_trialmean[uniq_trial_trialtype%in%OKtrials4pose,])
    assign('OKtrials4pose',OKtrials4pose, envir=.GlobalEnv)
    assign('OKtrials4aversion',OKtrials4aversion, envir=.GlobalEnv)
    assign('pose_flymean', pose_flymean, envir=.GlobalEnv)
   
    # calc puff effect from fly mean
    print('calculating puff effect...')
    # remove CALIB
    pose_flymean = subset(pose_flymean, trialtype=='normal')
    uniq.myset = pose_flymean$uniq %>% unique
    label.set = unique(pose_flymean$label)
    pose_puffeffect = 
        lapply(1:length(uniq.myset), function(k){
             subdata = subset(pose_flymean, uniq==uniq.myset[k])
             subdata.dcast = reshape2::dcast(subdata, genotype+npuff+uniq+trialtype+postPuffInterval+host~label, value.var='ratio')
             if(nrow(subdata.dcast)>1 & any(subdata.dcast$npuff==0)){
                 out.h = subset(subdata.dcast,
                              select=c(genotype, npuff, uniq, trialtype, postPuffInterval, host))[1:length(unique(subdata.dcast$npuff)),]
                 tmp = subset(subdata.dcast, select=-c(genotype, npuff, uniq, trialtype, postPuffInterval, host))
                 out.h.tmp = lapply(1:ncol(tmp), function(m){
                            tmp[,m] - tmp[subdata.dcast$npuff==0, m]
                              }) %>% bind_cols
                 colnames(out.h.tmp) = colnames(tmp)
                 out.h.tmp = out.h.tmp %>% as.list %>% as.data.frame
                 cbind(out.h, out.h.tmp) %>% {subset(., npuff!=0)}
             }
        }) %>% bind_rows
    pose_puffeffect.melt = reshape2::melt(pose_puffeffect, id.vars=c('genotype','npuff','uniq','trialtype','postPuffInterval', 'host'), value.name='ratio', variable.name='label')
    assign('pose_puffeffect', pose_puffeffect.melt, envir=.GlobalEnv)

    # select OK flies
    OKflies = 
        ifelse(
                any(grep('flight', pose_flymean$label)),
                subset(pose_flymean, trialtype=='normal' & npuff==0) %>%
                {reshape2::dcast(., uniq+genotype~label, value.var='ratio')} %>%
                {subset(., walk<0.25 & flight<0.2, select=uniq)} %>%
                as.list %>%
                as.vector,
                subset(pose_flymean, trialtype=='normal' & npuff==0) %>%
                {reshape2::dcast(., uniq+genotype~label, value.var='ratio')} %>%
                {subset(., walk<0.25, select=uniq)} %>%
                as.list %>%
                as.vector
                )[[1]]
    assign('OKflies', OKflies, envir=.GlobalEnv)

    print('all data loaded successfully!')
}
