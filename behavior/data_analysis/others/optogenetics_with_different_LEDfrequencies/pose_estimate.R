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
labels = c('flight','walk','stop','glooming','PER','freezeInAir','stuck')
nlabel = length(labels)
ethogram_colors <<- c('flight'='yellow1','walk'='red','glooming'='deepskyblue1','glooming_thorax'='magenta','PER'='blue','stop'='gray70','freezeInAir'='black')
ethogram_colors4smooth <<- c('flight'='gold3','walk'='red','glooming'='deepskyblue1','glooming_thorax'='magenta','PER'='blue','stop'='gray70','freezeInAir'='black')

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
	myset = unique(pose_trialmean$uniq_trialtype_LEDfreq)
	pose_flymean = NULL 
	for(x in 1:length(myset)){
		for(p in unique(pose_trialmean$LEDfreq)){
			subdata = subset(pose_trialmean, uniq_trialtype_LEDfreq==myset[x] & LEDfreq==p)
			if(nrow(subdata)==0) next
			myratio = as.data.frame(lapply(subdata[,(ncol(subdata)-nlabel+1):ncol(subdata)], mean))
			pose_flymean = rbind(pose_flymean, cbind(subdata[1,c('genotype','LEDfreq','uniq','trialtype','postPuffInterval','uniq_trialtype_LEDfreq')], myratio))
		}
	}
	pose_flymean = reshape2::melt(pose_flymean, id.vars=colnames(pose_flymean)[1:(ncol(pose_flymean)-nlabel)], variable.name='label', value.name='ratio')
	return(pose_flymean)
}

calc_puffeffect = function(pose_flymean){
    uniq.set = unique(pose_flymean$uniq)
    out = 
        lapply(1:length(uniq.set), function(u){
           pose_flymean.u = subset(pose_flymean, uniq==uniq.set[u])
           LEDfreq.set = unique(pose_flymean.u$LEDfreq)
           LEDfreq.set = LEDfreq.set[LEDfreq.set!=0]
           lapply(1:length(LEDfreq.set), function(L){
              pose_flymean.L = subset(pose_flymean.u, LEDfreq%in%c(0,LEDfreq.set[L]))
              label.set = unique(pose_flymean.l$label)
              lapply(1:length(label.set), function(l){
                  pose_flymean.l = subset(pose_flymean.L, label==label.set[l])
                  out.l = pose_flymean.l[1,]
                  out.l$ratio = with(pose_flymean.l, ratio[LEDfreq==LEDfreq.set[L]] - ratio[LEDfreq==0])
                  out.l$LEDfreq = LEDfreq.set[L]
                  out.l
               }) %>% bind_rows
            }) %>% bind_rows
        }) %>% bind_rows
    return(out)
}

readfiles = function(mypath, mypattern){
    myfiles = list.files(path=mypath, pattern=mypattern)
    alldata = lapply(1:length(myfiles), function(myfile){
                         read.csv(paste0(mypath,myfiles[myfile]), header=T)
               }) %>% bind_rows()
    return(alldata)
}

selectOKtrials4pose = function(pose_trialmean){
	for(mycol in c('freezeInAir','stuck')){
		if(any(grep(mycol, colnames(pose_trialmean)))){
			pose_trialmean = pose_trialmean[pose_trialmean[,mycol]<0.1, ]
		}
	}
    pose_trialmean$uniq_trial_trialtype %>%
        return
}

selectOKtrials4aversion = function(pose_trialmean){
	for(mycol in c('freezeInAir','stuck', 'flight')){
		if(any(grep(mycol, colnames(pose_trialmean)))){
			pose_trialmean = pose_trialmean[pose_trialmean[,mycol]<0.1, ]
		}
	}
    pose_trialmean$uniq_trial_trialtype %>%
        return
}

ethogram = function(pose_raw){
    myplot = 
        ggplot(pose_raw, aes(t, uniq_trial_trialtype, fill=label)) +
           theme_bw() +
           theme(axis.text.y=element_blank(), text=element_text(size=20)) +
           geom_tile() +
           labs(y='trials', x='time (s)', title=unique(pose_raw$LEDfreq)) +
           scale_fill_manual(values=ethogram_colors)
       return(myplot)
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
    
    # load trial mean for select time frames (done by python)
    print('loading pose_trialmean...')
    pose_trialmean = readfiles(mypath=paste0(statpath,'/trialmean/'), mypattern='.csv$')
    pose_trialmean$label = gsub('NA.s','NA',pose_trialmean$label)
	pose_trialmean$uniq_trial_trialtype = with(pose_trialmean, paste(uniq, trial, trialtype, sep='_'))
	pose_trialmean$uniq_trialtype_LEDfreq = with(pose_trialmean, paste(uniq, trialtype, LEDfreq, sep='_'))
	pose_trialmean = reshape2::dcast(pose_trialmean, filename+genotype+pufffreq+trial+uniq+LEDfreq+postPuffInterval+trialtype+uniq_trial_trialtype+uniq_trialtype_LEDfreq~label, value.var='ratio')
    assign('pose_trialmean', pose_trialmean, envir=.GlobalEnv)
    print('[*] success')

    # calc fly mean from trial mean (onset)
    print('calculating fly mean...')
    OKtrials4pose = selectOKtrials4pose(pose_trialmean)
    OKtrials4aversion = selectOKtrials4aversion(pose_trialmean)
    pose_flymean = calc_flymean(subset(pose_trialmean, uniq_trial_trialtype%in%OKtrials4pose))

    # calc puffeffect
#    pose_puffeffect = calc_puffeffect(pose_flymean)

    # save
    assign('OKtrials4pose',OKtrials4pose, envir=.GlobalEnv)
    assign('OKtrials4aversion',OKtrials4aversion, envir=.GlobalEnv)
    assign('pose_flymean', pose_flymean, envir=.GlobalEnv)
    assign('pose_puffeffect', pose_puffeffect, envir=.GlobalEnv)
    
    print('all data loaded successfully!')
}

statPosePuffEffect.inGeno = function(pose_puffeffect, p.adjust.method){
    out = NULL
    for(i in unique(pose_puffeffect$label)){
        for(g in unique(pose_puffeffect$genotype)){
            subpose_puffeffect = subset(pose_puffeffect, label==i & genotype==g)
            if(nrow(subpose_puffeffect)>0){
                shapiro = subpose_puffeffect$ratio %>% shapiro.test
                print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'pose',i,'_genotype',g,'_shapiro_stat.txt'), row.names=F, col.names=F)
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
    out = NULL
    for(l in 1:length(label.set)){
        pose_puffeffect.l = subset(pose_puffeffect, label==label.set[l])
        print(label.set[l])
        print(summary(aov(ratio~genotype, pose_puffeffect.l)))
        for(g in 1:length(genotype.set)){
            pose_puffeffect.g = subset(pose_puffeffect.l, genotype%in%c(control, genotype.set[g]))
            shapiro_ctrl = shapiro.test(pose_puffeffect.g$ratio[pose_puffeffect.g$genotype==control])$p.value
            shapiro_test = shapiro.test(pose_puffeffect.g$ratio[pose_puffeffect.g$genotype==genotype.set[g]])$p.value

            if(shapiro_ctrl >= 0.05 & shapiro_test>= 0.05){
                mytest = t.test(ratio ~ genotype, data = pose_puffeffect.g)
                effsize = cohen.d(ratio ~ genotype, data = pose_puffeffect.g)$estimate
            } else{
                mytest = wilcox.test(ratio ~ genotype, data = pose_puffeffect.g)
                effsize = with(pose_puffeffect.g, wilcoxonRG(ratio, genotype))
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

