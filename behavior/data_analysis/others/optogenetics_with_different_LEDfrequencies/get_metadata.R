
addMetaData = function(mydf, filename){
    # get metadata
    filename_split = unlist(strsplit(filename,'/'))
    jsonfile = paste(paste(head(filename_split, length(filename_split)-2), collapse='/'),'metadata',gsub('.csv','.json',tail(filename_split,1)), sep='/')
    metadata_all = rjson::fromJSON(file=jsonfile) # mind the <<- ... it is now a global variable
    if(is.null(metadata_all$pufffreq)) metadata_all$pufffreq = 0 # to handle "CALIB" jsonfiles
    metadata_all['uniq'] = paste(metadata_all[c('genotype','ID','date')], collapse='_')
    metadata_all['filename'] = filename
    metadata <<- metadata_all[names(metadata_all) %ni% c('bgcolor','objectcolor','serverscript','clientscript')] # <- strip unwanted info
    metanames <<- names(metadata)
    
    # add to dataframe
    for(i in names(metadata)){
        mydf[,i] = metadata[i]
    }

    return(mydf)
}

getMetaNames = function(filename){
    # get metadata
    filename_split = unlist(strsplit(filename,'/'))
    jsonfile = paste(paste(head(filename_split, length(filename_split)-2), collapse='/'),'metadata',gsub('.csv','.json',tail(filename_split,1)), sep='/')
    metadata_all = rjson::fromJSON(file=jsonfile)
    metadata_all['uniq'] = paste(metadata_all[c('genotype','ID','date')], collapse='_')
    metadata_all['filename'] = filename
    metadata <<- metadata_all[names(metadata_all) %ni% c('bgcolor','objectcolor','serverscript','clientscript')] # <- strip unwanted info
    metanames <<- names(metadata)
    return(metanames)
}

