##############################
# load libaries
##############################
source('./init.R')
source('./init_b-v-b-p-b-p-v-b.R')
#source('./init_b-p-v-b-p-b-v-b.R')
source('./basic.R')

plotdir = './plot/'

##################################
# Basic calc for one dir
##################################
# calc and save
batchcalc(gsub('export/','', outdir))

##################################
# Load results
##################################
# list dirs (if pooling multiple schedules)
mydirs = 
    lapply(1:length(outdirs), function(k){
       paste0(list.dirs(path=gsub('export/','', outdirs[k]), recursive=FALSE), '/GminusR/aligned/')
    }) %>% c %>% unlist
mydirs = mydirs[-grep('export', mydirs)]

# list dirs (from a single schedule)
mydirs = paste0(list.dirs(path=gsub('export/','', outdir), recursive=FALSE), '/GminusR/aligned/')
mydirs = mydirs[-grep('export', mydirs)]

# preprocessing
mydirs = mydirs[grep('ID', mydirs)]
mydirs = gsub('//','/', mydirs)

# load result
mydata = 
    lapply(1:length(mydirs), function(k){
       read.csv(paste0(mydirs[k], 'mydata.csv'))
    }) %>% bind_rows
mydata.bin = 
    lapply(1:length(mydirs), function(k){
       read.csv(paste0(mydirs[k], 'mydata.bin.csv'))
    }) %>% bind_rows
tmp = str_split_fixed(mydata.bin$ID, '_', 3)
mydata.bin$ID_base = paste(tmp[,1], tmp[,2], sep='_')
mydata.bin$TRIAL = gsub('00','',tmp[,3]) %>% as.numeric()
mydata.flyROImean = 
    lapply(1:length(mydirs), function(k){
       if(file.exists(paste0(mydirs[k], 'mydata.flyROImean.csv')))
           read.csv(paste0(mydirs[k], 'mydata.flyROImean.csv'))
    }) %>% bind_rows
tmp = mydata.flyROImean$ID_ROI %>% str_split_fixed('_',4)
mydata.flyROImean$ID = paste(tmp[,1], tmp[,2], sep='_')
change = 
    lapply(1:length(mydirs), function(k){
       if(file.exists(paste0(mydirs[k], 'change.csv')))
           read.csv(paste0(mydirs[k], 'change.csv'))
    }) %>% bind_rows
tmp = change$ID_ROI %>% str_split_fixed('_',4)
change$ID = paste(tmp[,1], tmp[,2], sep='_')
write.csv(change %>% subset(phase%in%c('visual.base','puff'), select=c(ID_ROI,phase,precedingz,zscore)), '/media/masato/CaImaging_1/file/data/FR/change_raw.csv', row.names=F)

##################################
# heatmap sorted by peak timing
##################################
# posi neurons
ID_ROI.sub = subset(mydata.flyROImean, phase=='puff' & zscore > 0)$ID_ROI
mydata.dcast = dcast(subset(mydata.bin, X>=5 & X<75 & ID_ROI%in%ID_ROI.sub, select=c(X, ID_ROI, zscore)), formula=ID_ROI~X, value.var='zscore')
mydata.matrix = as.matrix(subset(mydata.dcast, select=-ID_ROI))
rownames(mydata.matrix) = mydata.dcast$ID_ROI
mydata.matrix = na.omit(mydata.matrix)
maxtiming = apply(mydata.matrix, 1, findmax)
mydata.matrix = mydata.matrix[order(maxtiming),]
png(paste0('./plots/heatmap_b-v-b-p-b-p-v-b_posi.png'), height=5, width=6, units='in', res=300)
heatmap.2(mydata.matrix, scale='none', col=jet.colors, trace='none', density.info='none', dendrogram='none', Rowv=F, Colv=F)
dev.off()

# nega neurons
ID_ROI.sub = subset(mydata.flyROImean, phase=='puff' & zscore < 0)$ID_ROI
mydata.dcast = dcast(subset(mydata.bin, X>=5 & X<75 & ID_ROI%in%ID_ROI.sub, select=c(X, ID_ROI, zscore)), formula=ID_ROI~X, value.var='zscore')
mydata.matrix = as.matrix(subset(mydata.dcast, select=-ID_ROI))
rownames(mydata.matrix) = mydata.dcast$ID_ROI
mydata.matrix = na.omit(mydata.matrix)
maxtiming = apply(mydata.matrix, 1, findmax)
mydata.matrix = mydata.matrix[order(maxtiming),]
png(paste0('./plots/heatmap_b-v-b-p-b-p-v-b_nega.png'), height=5, width=6, units='in', res=300)
heatmap.2(mydata.matrix, scale='none', col=jet.colors, trace='none', density.info='none', dendrogram='none', Rowv=F, Colv=F)
dev.off()

##################################
# mean intensity along time
##################################
ggplot(mydata.bin, aes(X, dF)) +
    labs(x='', y='') +
    geom_hline(yintercept = 0) +
    geom_rect(data=puffwindow, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='white', alpha=.2, fill='red') + 
    geom_rect(data=visualwindow, mapping=aes(NULL, NULL, xmin=x1, xmax=x2, ymin=y1, ymax=y2), color='white', alpha=.2, fill='blue') + 
    geom_smooth(stat='summary', fun.data=mean_cl_boot, se=T, col='black') +
    theme_classic() + theme(text=element_text(size=25))

# plot example cells (puff response)
tmp = subset(mydata.bin, X>=25 & X<65 & ID_ROI%in%c('20210210_ID0_2_1','20210211_ID0_1_3','20210211_ID1_1_3')) #puff
tmp$X = tmp$X - 25
tmp$col = '1'
tmp$col[tmp$X>=20] = '2'
tmp$X = tmp$X %% 20
ggplot(tmp, aes(X, zscore)) + #%in%ID_ROI.sub
    facet_grid(ID_ROI~.) +
    geom_rect(data=puffwindow[1,], mapping=aes(NULL, NULL, xmin=5, xmax=15, ymin=y1, ymax=y2), color='white', alpha=.2, fill='red') + 
    geom_smooth(stat='summary', fun.data=mean_cl_boot, se=T, aes(color=col), lwd=2) +
    scale_color_manual(values=c('1'='black', '2'='gray')) +
    theme_classic() + theme(text=element_text(size=25), legend.position='None')

##################################
# change
##################################

# test normality
mydata.flyROImean %>%
    group_by(phase) %>%
    shapiro_test(zscore) %>%
    print %>%
    write.csv(paste0(plotdir, 'mydata.flyROImean_shapiro.csv'), row.names=F)
change %>%
    group_by(phase) %>%
    shapiro_test(precedingz) %>%
    print %>%
    write.csv(paste0(plotdir, 'change_precedingz_shapiro.csv'), row.names=F)

# plot (median, as distributions are non-normal
change.melt = reshape2::melt(change %>% subset(phase%in%c('puff','visual.base'), select=c(ID_ROI, phase,zscore,precedingz)), id.vars=c('ID_ROI','phase'), variable.name='type')
change.melt$type = factor(change.melt$type, levels=c('precedingz','zscore'))
ggplot(change.melt %>% subset(phase=='puff'), aes(type, value, color=type, fill=type)) + 
    theme_classic() +
    theme(text = element_text(size=20), legend.position='None') +
    scale_color_manual(values=c('precedingz'='black', 'zscore'='red')) +
    scale_fill_manual(values=c('precedingz'='black', 'zscore'='red')) +
    ylim(c(-5,5)) +
    geom_line(col='gray40', alpha=.2, aes(group=ID_ROI)) +
    stat_summary(fun.y=median, fun.ymin=median, fun.ymax=median, geom='crossbar', width=.7)
ggsave(paste0(plotdir, 'mydata.flyROImean_precedingframe-vs-puff.png'), height=5, width=2, dpi=300)

ggplot(change.melt %>% subset(phase=='visual.base'), aes(type, value, color=type, fill=type)) + 
    theme_classic() +
    theme(text = element_text(size=20), legend.position='None') +
    scale_color_manual(values=c('precedingz'='black', 'zscore'='blue')) +
    scale_fill_manual(values=c('precedingz'='black', 'zscore'='blue')) +
    ylim(c(-5,5)) +
    geom_line(col='gray40', alpha=.2, aes(group=ID_ROI)) +
    stat_summary(fun.y=median, fun.ymin=median, fun.ymax=median, geom='crossbar', width=.7)
ggsave(paste0(plotdir, 'mydata.flyROImean_precedingframe-vs-visual.base.png'), height=5, width=2, dpi=300)

# stat

# check normality
change %>%
    group_by(phase) %>%
    shapiro_test(zscore) %>% 
    print %>%
    write.csv(paste0(plotdir, 'change_shapiro.csv'), row.names=F)

change.visual.base = subset(change, phase=='visual.base')
mytest.visual.base = t.test(change.visual.base$zscore)
effsize.visual.base = with(change.visual.base, mean(zscore)/sd(zscore))
mytest.df.visual.base = with(mytest.visual.base, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t %>% as.data.frame
colnames(mytest.df.visual.base) = c('method','parameter','statistic','p.value')
mytest.df.visual.base$effsize = effsize.visual.base
mytest.df.visual.base$phase = 'visual.base'

change.puff = subset(change, phase=='puff')
mytest.puff = wilcox.test(change.puff$zscore)
mytest.puff[sapply(mytest.puff, is.null)] = NA
effsize.puff = wilcoxonOneSampleRC(change.puff$zscore, mu=0)
mytest.df.puff = with(mytest.puff, c(method, parameter, statistic, p.value)) %>% as.data.frame %>% t %>% as.data.frame
colnames(mytest.df.puff) = c('method','parameter','statistic','p.value')
mytest.df.puff$effsize = effsize.puff
mytest.df.puff$phase = 'puff'

mytest.df = rbind(mytest.df.visual.base, mytest.df.puff)
mytest.df$p.adjusted = mytest.df$p.value %>% p.adjust(method = 'bonferroni')
print(mytest.df)
write.csv(mytest.df, paste0(plotdir, 'change_stat.csv'), row.names=F)



# ratio of posi responders to puff and/or visual
mythresh = 1
puffeffect.dcast = dcast(subset(puffeffect, select=-dF), formula=ID_ROI~phase, value.var='zscore')
ID_ROI.puff = subset(puffeffect.dcast, (base.puff >= mythresh) | (puff >= mythresh) | (puff.previsual >= mythresh))$ID_ROI
ID_ROI.visual = subset(puffeffect.dcast, (visual.base >= mythresh))$ID_ROI
out = data.frame(profile = c('puff','visual','puffANDvisual'),
                 ratio = c(
                           sum(ID_ROI.puff%ni%ID_ROI.visual),
                           sum(ID_ROI.visual%ni%ID_ROI.puff),
                           sum(ID_ROI.puff%in%ID_ROI.visual)
                           )
                 )
out$ratio = out$ratio / nrow(puffeffect.dcast)
out = rbind(out, data.frame(profile='other', ratio = (1 - sum(out$ratio))))
print(out)

out$profile = factor(out$profile, levels=c('puff','puffANDvisual','visual','other'))
ggplot(out, aes('', ratio, fill=profile)) +
    theme_void() +
    theme(legend.position='None') +
    scale_fill_manual(values=c('puff'='red','puffANDvisual'='magenta','visual'='blue','other'='gray')) +
    geom_bar(stat="identity", width=1, color='white', lwd=1) +
    coord_polar("y", start=0, direction=-1)
