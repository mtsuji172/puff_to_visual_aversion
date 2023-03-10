#!/usr/bin/Rscript
# Rscript colorcode.R ../Tk-GCaMP6s/

library(raster) # need sudo apt-get install libgdal-dev in system and install.packages('rgdal') in R
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(cetcolor)

# accept arguments
args = commandArgs(trailingOnly=T)
jet.colors = colorRampPalette(c("darkblue", "blue", "deepskyblue3", "mediumseagreen", "orange", "yellow", "lemonchiffon"))
colorcode = function(myflies, outdirname, ROI = c(10,490,10,490), keyT=NULL, keyF=NULL){ # dir of single fly, outdirname (e.g. "colorcoded_change")
    for(myfly in myflies){
        # make dir
        myfly.split = unlist(strsplit(myfly, '/'))
        fullpath2outdir = paste0(paste(myfly.split[1:(length(myfly.split)-1)],collapse='/'), '/', outdirname)
        dir.create(file.path(fullpath2outdir), showWarnings=F)
        myfiles = list.files(path=myfly, pattern='*.tif$')
        if(!is.null(keyT)) myfiles = myfiles[grep(keyT, myfiles)]
        if(!is.null(keyF)) myfiles = myfiles[-grep(keyF, myfiles)]
        
        print(paste0('importing images from ',myfly,' ...'))
        myrasters = list(NULL)
        mymin = NULL
        mymax = NULL
        for(i in 1:length(myfiles)){
            myraster = crop(raster(paste0(myfly,'/',myfiles[i])), extent(ROI))
            myrasters[[i]] = myraster
            mymin[i] = min(getValues(myraster))
            mymax[i] = max(getValues(myraster))
        }
        
        print('exporting plots...')
        for(i in 1:length(myrasters)){
            plot = gplot(crop(myrasters[[i]], extent(ROI)), maxpixels=length(crop(myrasters[[i]], extent(ROI)))) + geom_tile(aes(fill=(value-1)*100)) + scale_fill_gradientn(colours=jet.colors(7), limits=(c(min(mymin), max(mymax))-1)*100) + theme_bw()
            ggsave(plot=plot, filename=paste0(fullpath2outdir,'/',gsub('.tif','_colorcoded.tiff',myfiles[i])), height=5, width=6,dpi=500)
        }
        print('done')
    }
}

# main
ROI = c(20,480,20,480)
myflies = list.dirs(args[1], recursive=F) # -> ['ID0','ID1',...]
myflies = myflies[grep('ID', myflies)]
myflies_original = paste0(myflies, '/zprojection/aligned/')
myflies_change = paste0(myflies, '/zprojection/aligned_change/')
colorcode(myflies, 'colorcode_average_change/', ROI = ROI, keyT='change')
colorcode(myflies, 'colorcode_average/', ROI = ROI, keyF='change')
colorcode(myflies_original, "colorcode",ROI)
colorcode(myflies_change, "colorcoded_change",ROI)
