
library(ggplot2)
library(reshape2)
library(stringr)
library(gexport)
library(dendextend)
library(dplyr)
library(ggrepel)
library(gridExtra)
library(corrr)
library(corrplot)
library(foreach)
library(doParallel)

# params
sessionDuration = 30 #[s]
binsize = 0.5 #[s]
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
jet.colors = colorRampPalette(c("darkblue", "blue", "deepskyblue3", "mediumseagreen", "orange", "yellow", "lemonchiffon"))

findmax = function(x){
    return(which(x==max(x))[1])
}

distfunc = function(x){
    return(as.dist((1-cor(t(x)))/2))
}

prep4heatmap = function(){
    # prep for heatmap
    mydata.binmean.sub = mydata.binmean
    condition = rep('ctrl', nrow(mydata.binmean.sub))
    condition[grep('glue', mydata.binmean.sub$ID)] = 'a3glued'
    condition[grep('cut', mydata.binmean.sub$ID)] = 'a3cut'
    mydata.binmean.sub$condition = condition; rm(condition)
    mydata.binmean.sub$condition = factor(mydata.binmean.sub$condition, levels=c('ctrl','a3glued','a3cut','asahina'))
    mydata.binmean.sessionwise.sub = mydata.binmean.sessionwise
    condition = rep('ctrl', nrow(mydata.binmean.sessionwise.sub))
    condition[grep('glue', mydata.binmean.sessionwise.sub$ID)] = 'a3glued'
    condition[grep('cut', mydata.binmean.sessionwise.sub$ID)] = 'a3cut'
    mydata.binmean.sessionwise.sub$condition = condition; rm(condition)
    mydata.binmean.sessionwise.sub$condition = factor(mydata.binmean.sessionwise.sub$condition, levels=c('ctrl','a3glued','a3cut','asahina'))

	# hierarchical clustering
    mydata.binmean.dcast = dcast(subset(mydata.binmean, select=c(X, ID_ROI, zscore)), formula=ID_ROI~X, value.var='zscore')
    mydata.binmean.matrix = as.matrix(subset(mydata.binmean.dcast, select=-ID_ROI))
    rownames(mydata.binmean.matrix) = mydata.binmean.dcast$ID_ROI
    mydata.binmean.matrix = na.omit(mydata.binmean.matrix)
    d = distfunc(mydata.binmean.matrix)
    hr = hclust(d,method='complete')
    mycl = cutree(hr, h=max(hr$height/1.5))

    # export
    assign('mydata.binmean.sub', mydata.binmean.sub, envir=.GlobalEnv)
    assign('mydata.binmean.sessionwise.sub', mydata.binmean.sessionwise.sub, envir=.GlobalEnv)
    assign('mydata.binmean.matrix', mydata.binmean.matrix, envir=.GlobalEnv)
    assign('d', d, envir=.GlobalEnv)
    assign('hr', hr, envir=.GlobalEnv)
    assign('mycl', mycl, envir=.GlobalEnv)
}


batchcalc = function(mypath=){
    # list dirs
    mydirs = paste0(list.dirs(path=mypath, recursive=FALSE), '/zprojection/aligned')
    mydirs = mydirs[grep('ID', mydirs)]

    # load result
    print('loading results...')
    mydata = NULL
    for(mydir in mydirs){
        subdata = try(read.csv(paste0(mydir,'/Results.csv')), silent=F)
        if (class(subdata) == "try-error") next
        
        # normalize the time (each fly required different depth along z to scan the VGluT+Tk cluster, requiring different duration for each scan along z.
        subdata$X = round((subdata$X-1) * sessionDuration / max(subdata$X-1), 2)
        
        # select signal columns
        subdata = subdata[,c(1,grep('Mean',colnames(subdata)))]

        # melt
        subdata = melt(subdata, id.vars='X', variable.name='ROI', value.name='intensity')
        subdata = subset(subdata, X>=5)
        
        # detrend
        linear_fit = lm(intensity~X, data=subdata)
        subdata$intensity = subdata$intensity - linear_fit$fitted.values + linear_fit$fitted.values[1]
    
        # add info
        subdata$ID = tail(unlist(strsplit(mydir,'/')),3)[1]
        subdata$condition = 'ctrl'
        subdata$condition[grep('a3glue', subdata$ID)] = 'a3glue'; subdata$condition[grep('a3cut', subdata$ID)] = 'a3cut'
        subdata$condition = factor(subdata$condition, levels=c('ctrl','a3glue','a3cut'))
        mydata = rbind(mydata, subdata)
    }
    print('results loaded successfully!')

    # each fly requires different duration for one bin of z-scan => bin time (for each ROI, take mean if measured repeatedly)
    print('binning data...')
    mydata$ROI = gsub('Mean','',mydata$ROI)
    breaks = seq(0,sessionDuration,by=binsize)
    tags = seq(0,sessionDuration-binsize,by=binsize)
    group_tags = cut(mydata$X, breaks=breaks, include.lowest=T, right=F, labels=tags)
    group_tags.numeric = as.numeric(as.character(group_tags))
    mydata$X = group_tags.numeric

    # add info
    tmp = str_split_fixed(mydata$ID,'_',3)[,1:3]
    mydata$ID = paste(tmp[,1],tmp[,2],sep='_')
    mydata$session = tmp[,3]
    mydata$session[mydata$session==''] = '0'
    rm(tmp)
    mydata$ID_ROI = with(mydata, paste0(gsub('Mean','',ID), '_', ROI))
    mydata$ID_ROI_X = with(mydata, paste0(ID_ROI, '_', X))
    mydata$ID_ROI_session_X = with(mydata, paste0(ID_ROI, '_', session, '_', X))

    cl = makeCluster(5, type="FORK")
    registerDoParallel(cl)
    ID_ROI_session_X.set = unique(mydata$ID_ROI_session_X)
    mydata.binmean.sessionwise = 
        foreach(k = 1:length(ID_ROI_session_X.set), .export=ls(.GlobalEnv), .combine='rbind') %dopar%{
            subdata = subset(mydata, ID_ROI_session_X == ID_ROI_session_X.set[k])
            out = subdata[1,]
            out$intensity = mean(subdata$intensity)
            out
        }
    stopCluster(cl)

    ID_ROI_X.set = unique(mydata.binmean.sessionwise$ID_ROI_X)
    mydata.binmean = 
        lapply(1:length(ID_ROI_X.set), function(k){
            subdata = subset(mydata.binmean.sessionwise, ID_ROI_X == ID_ROI_X.set[k])
            out = subdata[1,]
            out$intensity = mean(subdata$intensity)
            out
        }) %>% bind_rows
    print('done')

    # normalize the value for each ROI
    print('transforming intensities into z-score and deltaF...')
    mydata.binmean.sessionwise.normalized = NULL
    for(k in unique(mydata.binmean.sessionwise$ID_ROI)){
        for(h in unique(mydata.binmean.sessionwise$session)){
            subdata = subset(mydata.binmean.sessionwise, ID_ROI==k & session==h)
            if(nrow(subdata)==0) next
            subdata$zscore = with(subdata, (intensity - mean(intensity)) / sqrt(var(intensity)))
            if(!any(subdata$X==10)) {
                subdata$F = with(subdata, ((intensity - mean(intensity[X%in%c(9.5,10.5)])) / mean(intensity[X%in%c(9.5,10.5)]))*100)
            } else{
                subdata$F = with(subdata, ((intensity - intensity[X==10]) / intensity[X==10])*100)
            }
            mydata.binmean.sessionwise.normalized = rbind(mydata.binmean.sessionwise.normalized, subdata)
        }
    }
    mydata.binmean.sessionwise = mydata.binmean.sessionwise.normalized; rm(mydata.binmean.sessionwise.normalized)
    mydata.binmean.sessionwise$X = as.numeric(as.character(mydata.binmean.sessionwise$X))
    
    mydata.binmean.normalized = NULL
    for(k in unique(mydata.binmean$ID_ROI)){
        subdata = subset(mydata.binmean, ID_ROI==k)
        subdata$zscore = with(subdata, (intensity - mean(intensity)) / sqrt(var(intensity)))
        if(!any(subdata$X==10)) {
            subdata$F = with(subdata, ((intensity - mean(intensity[X%in%c(9.5,10.5)])) / mean(intensity[X%in%c(9.5,10.5)]))*100)
        } else{
            subdata$F = with(subdata, ((intensity - intensity[X==10]) / intensity[X==10])*100)
        }
        mydata.binmean.normalized = rbind(mydata.binmean.normalized, subdata)
    }
    mydata.binmean = mydata.binmean.normalized; rm(mydata.binmean.normalized)
    mydata.binmean$X = as.numeric(as.character(mydata.binmean$X))
    mydata.binmean = subset(mydata.binmean, select=-ID_ROI_session_X)
    print('done')

    # calc mean / puffcondition / fly_ROI and session
    print('calculating mean per puff condition per ROI per session...')
    mydata.flyROImean.sessionwise = NULL
	for(myID_ROI in unique(mydata.binmean.sessionwise$ID_ROI)){
        for(h in unique(mydata.binmean.sessionwise$session)){
            subdata = subset(mydata.binmean.sessionwise, ID_ROI==myID_ROI & session==h)
            if(nrow(subdata)==0) next

            # puff "OFF prior"
            subsubdata = subset(subdata, (X>=5 & X<10))
            tmp = subsubdata[1,-which(colnames(subsubdata)%in%c('X','condition'))]
            tmp$intensity = max(subsubdata$intensity)
            tmp$zscore = max(subsubdata$zscore)
            tmp$puff= '-prior'
            mydata.flyROImean.sessionwise = rbind(mydata.flyROImean.sessionwise, subset(tmp,select=-ID_ROI_X))
            
            # puff "ON"
            subsubdata = subset(subdata, (X>10 & X<=20))
            tmp = subsubdata[1,-which(colnames(subsubdata)%in%c('X','condition'))]
            tmp$intensity = max(subsubdata$intensity)
            tmp$zscore = max(subsubdata$zscore)
            tmp$puff= '+'
            mydata.flyROImean.sessionwise = rbind(mydata.flyROImean.sessionwise, subset(tmp,select=-ID_ROI_X))
            
            # puff "OFF post"
            subsubdata = subset(subdata, (X>=23))
            tmp = subsubdata[1,-which(colnames(subsubdata)%in%c('X','condition'))]
            tmp$intensity = max(subsubdata$intensity)
            tmp$zscore = max(subsubdata$zscore)
            tmp$puff= '-post'
            mydata.flyROImean.sessionwise = rbind(mydata.flyROImean.sessionwise, subset(tmp,select=-ID_ROI_X))
        }
	}
    print('done')

    # calc puffeffect for each fly_ROI and session
    print('calculating puff effect per ROI per session...')
    puffeffect.sessionwise = NULL
    for(i in unique(mydata.flyROImean.sessionwise$ID_ROI)){
        for(h in unique(mydata.flyROImean.sessionwise$session)){
            subdata = subset(mydata.flyROImean.sessionwise, ID_ROI==i & session==h)
            if(nrow(subdata)==0) next
            subpuffeffect.sessionwise = subdata[1,-which(colnames(subdata)=='puff')]
            subpuffeffect.sessionwise$intensity = with(subdata, (intensity[puff=='+'] - intensity[puff=='-prior']) / intensity[puff=='-prior'] * 100)
            subpuffeffect.sessionwise$intensity_persistence = with(subdata, (intensity[puff=='-post'] - intensity[puff=='-prior']) / intensity[puff=='-prior'] * 100)
            subpuffeffect.sessionwise$zscore = with(subdata, zscore[puff=='+']-zscore[puff=='-prior'])
            subpuffeffect.sessionwise$zscore_persistence = with(subdata, zscore[puff=='-post'] - zscore[puff=='-prior'])
            puffeffect.sessionwise = rbind(puffeffect.sessionwise, subpuffeffect.sessionwise)
        }
    }
    print('done')
    
    # calc mean / puffcondition / fly_ROI
    print('calculating mean per puff condition per ROI...')
    mydata.flyROImean = NULL
	for(myID_ROI in unique(mydata.binmean$ID_ROI)){
		subdata = subset(mydata.binmean, ID_ROI==myID_ROI & session%in%c(0,1,2))
		# puff "OFF prior"
		subsubdata = subset(subdata, (X>=5 & X<10))
		tmp = subsubdata[1,-which(colnames(subsubdata)%in%c('X','condition'))]
		tmp$intensity = max(subsubdata$intensity)
		tmp$zscore = max(subsubdata$zscore)
		tmp$puff= '-prior'
		mydata.flyROImean = rbind(mydata.flyROImean, subset(tmp,select=-ID_ROI_X))
		
		# puff "ON"
		subsubdata = subset(subdata, (X>10 & X<=20))
		tmp = subsubdata[1,-which(colnames(subsubdata)%in%c('X','condition'))]
		tmp$intensity = max(subsubdata$intensity)
		tmp$zscore = max(subsubdata$zscore)
		tmp$puff= '+'
		mydata.flyROImean = rbind(mydata.flyROImean, subset(tmp,select=-ID_ROI_X))
		
        # puff "OFF post"
		subsubdata = subset(subdata, X>=23)
		tmp = subsubdata[1,-which(colnames(subsubdata)%in%c('X','condition'))]
		tmp$intensity = max(subsubdata$intensity)
		tmp$zscore = max(subsubdata$zscore)
		tmp$puff= '-post'
		mydata.flyROImean = rbind(mydata.flyROImean, subset(tmp,select=-ID_ROI_X))
		
	}
    print('done')

    # calc puffeffect for each fly_ROI
    print('calculating puff effect per ROI...')
    puffeffect = NULL
    for(i in unique(mydata.flyROImean$ID_ROI)){
        subdata = subset(mydata.flyROImean, ID_ROI==i)
        subpuffeffect = subdata[1,-which(colnames(subdata)=='puff')]
        subpuffeffect$intensity = with(subdata, (intensity[puff=='+'] - intensity[puff=='-prior']) / intensity[puff=='-prior'] * 100)
        subpuffeffect$intensity_persistence = with(subdata, (intensity[puff=='-post'] - intensity[puff=='-prior']) / intensity[puff=='-prior'] * 100)
        subpuffeffect$zscore = with(subdata, zscore[puff=='+']-zscore[puff=='-prior'])
        subpuffeffect$zscore_persistence = with(subdata, zscore[puff=='-post'] - zscore[puff=='-prior'])
        puffeffect = rbind(puffeffect, subpuffeffect)
    }
    puffeffect$condition = 'ctrl'
    puffeffect$condition[grep('a3glue', puffeffect$ID)] = 'a3glue'; puffeffect$condition[grep('a3cut', puffeffect$ID)] = 'a3cut'; puffeffect$condition[grep('asahina', puffeffect$ID)] = 'asahina'
    puffeffect$condition = factor(puffeffect$condition, levels=c('ctrl','a3glue','a3cut','asahina'))
    print('done')
    
    # assign results to .GlobalEnv
    print('assigning results to .GlobalEnv...')
    assign('mydata', mydata, envir=.GlobalEnv)
    assign('mydata.binmean', mydata.binmean, envir=.GlobalEnv)
    assign('mydata.flyROImean', mydata.flyROImean, envir=.GlobalEnv)
    assign('puffeffect', puffeffect, envir=.GlobalEnv)
    assign('mydata.binmean.sessionwise', mydata.binmean.sessionwise, envir=.GlobalEnv)
    assign('mydata.flyROImean.sessionwise', mydata.flyROImean.sessionwise, envir=.GlobalEnv)
    assign('puffeffect.sessionwise', puffeffect.sessionwise, envir=.GlobalEnv)
    print('done')
}


#################################
# main
#################################
noexecute = function(){
# batch calc
batchcalc('../GCaMP6s')
    
# heatmap sorted by peak timing
prep4heatmap()
maxtiming = apply(mydata.binmean.matrix, 1, findmax)
mydata.binmean.matrix = mydata.binmean.matrix[order(maxtiming),]
heatmap.2(mydata.binmean.matrix, scale='none', col=jet.colors, trace='none', density.info='none', dendrogram='none', Rowv=F, Colv=F)

# mean intensity along time
puffwindow = data.frame(y1=-Inf, y2=Inf, x1=10, x2=20, mycol=c('puff'))
ggplot(mydata.binmean, aes(X, F, color=condition, fill=condition)) + 
    theme_classic() + 
    geom_rect(data=puffwindow, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='white', alpha=.2, fill='red') +
    scale_color_manual(values=c('black','red','blue','green')) + 
    scale_fill_manual(values=c('black','red','blue','green')) +
    geom_smooth(stat='summary', fun.data=mean_cl_boot, alpha=.2) 

# puffeffect
ggplot(puffeffect, aes(condition, intensity, color=condition)) + 
    geom_hline(yintercept=0, lty=2) + 
    theme_classic() + 
    scale_color_manual(values=c('black','red','blue','green')) + 
    theme(legend.position='None') +
    stat_summary()

# stat
nsize.cell = puffeffect$condition %>% as.factor %>% summary %>% as.data.frame
condition.set = unique(puffeffect$condition)
nsize.fly = 
    lapply(1:length(condition.set), function(k){
               data.frame(condition = condition.set[k],
                          n = subset(puffeffect, condition==condition.set[k])$ID %>% unique %>% length)
        }) %>% bind_rows

mystat = 
    data.frame(
               condition = c('a3cut', 'a3glue'),
               pvalue = c(
                          t.test(intensity ~ condition, data=subset(puffeffect, condition%in%c('ctrl','a3cut')))$p.value,
                          t.test(intensity ~ condition, data=subset(puffeffect, condition%in%c('ctrl','a3glue')))$p.value)
               )
mystat$pvalue = p.adjust(mystat$pvalue, method='bonferroni')

}
