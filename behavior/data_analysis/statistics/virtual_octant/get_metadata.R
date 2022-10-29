
addMetaData = function(mydf, filename){
    # get metadata
    filename_split = unlist(strsplit(filename,'/'))
    jsonfile = paste(paste(head(filename_split, length(filename_split)-2), collapse='/'),'metadata',gsub('.csv','.json',tail(filename_split,1)), sep='/')
    metadata_all = rjson::fromJSON(file=jsonfile) # mind the <<- ... it is now a global variable
    metadata_all['uniq'] = paste(metadata_all[c('genotype','ID','date')], collapse='_')
    metadata <<- metadata_all[names(metadata_all) %in% c('date','host','genotype','uniq')]
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
    jsonfile = paste(paste(head(filename_split, length(filename_split)-2), collapse='/'),'metadata/',gsub('.csv','.json',tail(filename_split,1)), sep='/')
    metadata_all = rjson::fromJSON(file=jsonfile) # mind the <<- ... it is now a global variable
    metadata_all['uniq'] = paste(metadata_all[c('genotype','ID','date')], collapse='_')
    metadata_all['filename'] = filename
    metadata <<- metadata_all[names(metadata_all)  %in% c('date','host','genotype','uniq')]
    metanames <<- names(metadata)
    return(metanames)
}

