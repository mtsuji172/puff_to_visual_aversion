library(gridExtra)
homeDir = '../../data/main/CS_virtualoctant_puff/'
source('./init.R')
source('./calc_indices.R')

#######################
# params
#######################
mypath='../../data/main/CS_virtualoctant_puff/ballrecordings/'
parallel=T
maxtime = 60

#######################
# plot timecourse
#######################
### assemble all the csv files
myfiles = list.files(path=mypath, pattern='.csv$')
myfiles = paste0(mypath, myfiles[-grep('CALIB',myfiles)])
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
            preprocess(myfiles[i])
        })%>% bind_rows
}

# time course of heading direction
ggplot() +
    geom_vline(xintercept=-135, lty=1, size=.5, col='black') +
    geom_vline(xintercept=135, lty=1, size=.5, col='black') +
    geom_point(data=subset(mydf_all, t <= maxtime), aes(-angle, t, color=puff), alpha=.01, size=1) +
    facet_grid(.~genotype) +
    scale_color_manual(values=c('gray40','red')) +
    scale_x_continuous(breaks=seq(-135, 135, by=45), labels=seq(-135, 135, by=45), limits=c(-135,135)) +
    scale_y_reverse() +
    geom_rect(data=rects, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), fill='red', alpha=.1)+
    xlab('heading direction (degree)') +
    ylab('time (s)') +
    theme(legend.position='None')
ggsave('timecourse.png', height=4, width=4, dpi=300)

# time course of heading direction (in polar)
ggplot(mydf_all) +
    geom_point(data=mydf_all[seq(0,nrow(mydf_all),by=10),], aes(-angle, t, color=puff), alpha=.2, size=.005)  +
    scale_color_manual(values=c('gray40','red')) +
    facet_grid(.~genotype) +
    scale_x_continuous(breaks=seq(-180, 180, by=45), labels=seq(-180, 180, by=45), limits=c(-180,180)) +
    geom_rect(data=rects, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), fill='red', alpha=.1)+
    xlab('heading direction (degree)') +
    ylab('time (s)') +
    coord_polar(theta='x', start=pi)
ggsave('timecourse_polar.png', height=5, width=5, dpi=300)



##########################
# analyze indices
##########################

result_all = calcIndices(mydf_all %>% subset(t<=maxtime))
ggplot(result_all, aes(genotype, attractionIndex, color=genotype)) + 
    theme_classic() +
    theme(legend.position='None') +
    geom_hline(yintercept = c(0,1), lty=1, color='black', lwd=.5) +
    geom_hline(yintercept = 0.5, lty=3, color='black', lwd=.5) +
    geom_boxplot(alpha=.4, outlier.shape = NA) +
#    geom_jitter(height=0, width=.2) +
    scale_color_manual(values=c('gray40','red')) +
    ylim(values=c(0,1))
ggsave('attractionIndex.png', height=5, width=2, dpi=300)

# plot bout count
result_all.melt = select(result_all, -'attractionIndex') %>%
    reshape2::melt(id.vars=c('genotype','host','date','uniq'), variable.name='index', value.name='count')
ggplot(result_all.melt, aes(genotype, count, color=genotype, fill=genotype)) +
    facet_grid(.~index) +
    scale_color_manual(values=c('gray40','red')) +
    scale_fill_manual(values=c('gray40','red')) +
    geom_boxplot(alpha=.4, outlier.shape = NA) +
    geom_jitter(height=0, width=.2)

# plot diff of bout count
p1 = ggplot(result_all, aes(genotype, nPassThrough_diff, color=genotype, fill=genotype)) +
    scale_color_manual(values=c('gray40','red')) +
    scale_fill_manual(values=c('gray40','red')) +
    geom_hline(yintercept=0, lty=1, lwd=.5, col='black') +
    geom_boxplot(alpha=.4, outlier.shape = NA) +
    geom_jitter(height=0, width=.2) +
    labs(y='# rush-through / trial (ON - OFF)') +
    theme(legend.position = 'None')
p2 = ggplot(result_all, aes(genotype, nTurnAround_diff, color=genotype, fill=genotype)) +
    scale_color_manual(values=c('gray40','red')) +
    scale_fill_manual(values=c('gray40','red')) +
    geom_hline(yintercept=0, lty=1, lwd=.5, col='black') +
    geom_boxplot(alpha=.4, outlier.shape = NA) +
    geom_jitter(height=0, width=.2) +
    labs(y='# turn-around / trial (ON - OFF)') +
    theme(legend.position = 'None')
grid.arrange(p1, p2, ncol=2)



# make animation
compressionRate4animation = 10
outdir = 'animatedPlot/'
if(!file.exists(outdir)) system(paste0('mkdir ', outdir))

mydf = subset(mydf_all, filename == myfile)

loopNo = round(nrow(mydf) / compressionRate4animation, digits=0)
print(paste0('implementing ', loopNo, ' loops...'))
for(i in 1:loopNo){
    maxFrame_i = i * compressionRate4animation
    print(paste0(i, '/', loopNo))
    p = plotfunc(mydf[1:maxFrame_i,])
    p = p + ylim(0, max(mydf$t)*1.05)
    if(nchar(i) < nchar(loopNo)){
        zeroComplement = paste(rep('0', nchar(loopNo)-nchar(i)),collapse='')
        i_new = paste0(zeroComplement, i)
    } else{
        i_new = i
    }
    ggsave(paste0(outdir, i_new, '.png'), device='png')
}
