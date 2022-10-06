### common
trialduration = 130 # s
meta.set = c('DATE','GENOTYPE','DPE','ID','TRIAL')
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
base.window = c(10, 60)
puff.window = c(60, 70)
puffwindow = data.frame(y1=c(-Inf), y2=c(Inf), x1=puff.window[1], x2=puff.window[2], mycol=c('puff'))
mycolors_windowave = c('base'='black','puff'='red')

### sample-specific
mydir = './'
root = paste0('../data/',mydir)
plotdir = paste0('./plots/',mydir)
dir.create(plotdir, showWarnings=F, recursive=T)
genotype.set = 'HandGFP'
mycolors = c('HandGFP'='black')
