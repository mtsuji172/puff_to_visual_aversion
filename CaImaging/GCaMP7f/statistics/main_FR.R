##############################
# load libaries
##############################
source('init.R')
source('init_b-v-b-p-b-p-v-b.R')
source('basic.R')


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
puffeffect = 
    lapply(1:length(mydirs), function(k){
       if(file.exists(paste0(mydirs[k], 'puffeffect.csv')))
           read.csv(paste0(mydirs[k], 'puffeffect.csv'))
    }) %>% bind_rows
tmp = puffeffect$ID_ROI %>% str_split_fixed('_',4)
puffeffect$ID = paste(tmp[,1], tmp[,2], sep='_')


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
# puffeffect
##################################
ggplot(subset(puffeffect, phase%in%c('puff','visual.base')), aes(phase, zscore, color=phase, fill=phase)) + 
    theme_classic() +
    theme(text = element_text(size=20), legend.position='None') +
    geom_hline(yintercept=0) + 
    scale_color_manual(values=c('puff'='red', 'visual.base'='blue')) +
    scale_fill_manual(values=c('puff'='red', 'visual.base'='blue')) +
    stat_summary(size=1)
ggsave('./plots/fromLeft/Fig5f_puffeffect_jGCaMP7f_152Hz_visual5_b-v-b-p-b-p-v-b.png', height=5, width=3, dpi=300)

# stat (puff vs base, visual.base vs base)
mystat = 
    data.frame(comparison = c('puff-vs-base', 'visual.base-vs-base'),
               pvalue = c(subset(puffeffect, phase=='puff')$zscore %>% t.test %>% {.$p.value},
                          subset(puffeffect, phase=='visual.base')$zscore %>% t.test %>% {.$p.value}
                          ))
mystat$pvalue = p.adjust(mystat$pvalue, method='bonferroni')
print(mystat)

# stat (dpuff vs dvisual.base)
mystat = subset(puffeffect, phase%in%c('puff','visual.base')) %>% {t.test(dF ~ phase, data=.)} %>% {.$p.value}
print(mystat)

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
