library(gridExtra)
source('./init.R')
source('./calc_indices.R')

#######################
# params
#######################
root = '/media/masato/free/file/behavior/data/'
mydir = 'CS_virtualoctant_puff/'
dir.create(mydir, showWarnings=F)
mypath = paste0(root, mydir, 'ballrecordings/')
parallel=T

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
    geom_point(data=mydf_all, aes(-angle, t, color=puff), alpha=.01, size=1) +
    facet_grid(.~genotype) +
    scale_color_manual(values=c('gray40','red')) +
    scale_x_continuous(breaks=seq(-135, 135, by=45), labels=seq(-135, 135, by=45), limits=c(-135,135)) +
    scale_y_reverse() +
    geom_rect(data=rects, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), fill='red', alpha=.1)+
    xlab('heading direction (degree)') +
    ylab('time (s)') +
    theme(legend.position='None')
ggsave(paste0(mydir, 'timecourse.png'), height=4, width=4, dpi=300)


##########################
# analyze indices
##########################

result_all = calcIndices(mydf_all)
ggplot(result_all, aes(genotype, attractionIndex, color=genotype)) + 
    theme_classic() +
    theme(legend.position='None') +
    geom_hline(yintercept = c(0,1), lty=1, color='black', lwd=.5) +
    geom_hline(yintercept = 0.5, lty=3, color='black', lwd=.5) +
    geom_boxplot(alpha=.4, outlier.shape = NA) +
    scale_color_manual(values=c('gray40','red')) +
    ylim(values=c(0,1))
ggsave(paste0(mydir, 'attractionIndex3.png'), height=3, width=1.25, dpi=300)

# nsize
nsize = summary(as.factor(result_all$genotype)) %>% as.data.frame
print(nsize)
write.csv(nsize, paste0(mydir, 'nsize.csv'), row.names=F)

# stat
shapiro = result_all$attractionIndex %>% shapiro.test
print(shapiro) %>% capture.output %>% write.table(paste0(mydir, 'result_all_shapiro_stat.txt'), row.names=F, col.names=F)
result_all$attractionIndex %>% t.test %>% print %>% capture.output %>% write.table(paste0(mydir, 'result_all_stat.txt'), row.names=F, col.names=F)
