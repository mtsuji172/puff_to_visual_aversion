ONtime = c(10, 40)
visONtime = c(20, 60)
OFFtime = ONtime+10 # ONtime defined in init_XXX.R
visOFFtime = visONtime+10
puffwindow = data.frame(y1=c(-Inf), y2=c(Inf), x1=c(ONtime), x2=c(OFFtime), mycol=c('puff'))
visualwindow = data.frame(y1=c(-Inf), y2=c(Inf), x1=c(visONtime), x2=c(visOFFtime), mycol=c('vis'))
outdir = paste0(path2data, 'jGCaMP7f_152Hz_visual5_b-p-v-b-p-b-v-b/export/')
myschedule =
   list('base'=list(c(1,10)),
        'base.puff'=list(c(50,60)),
        'base.visual'=list(c(70,80)),
        'base.visual.postpuff'=list(c(30,40)),
        'visual.base'=list(c(60,70)),
        'visual.puff'=list(c(20,30)),
        'puff.previsual'=list(c(10,20)),
        'puff'=list(c(40,50)))
myschedule_onset = unlist(myschedule)[grep('1$', names(unlist(myschedule)))]
myschedule_order = gsub('1','',names(myschedule_onset))[order(myschedule_onset)]
