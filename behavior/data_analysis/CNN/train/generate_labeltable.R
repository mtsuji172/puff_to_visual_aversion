#!/usr/bin/Rscript
library(dplyr)

# params
path = 'path/to/dir/containing/images_mean/'

# list up dirs
dirs = list.dirs(path, recursive=F)
dirs = dirs[grep('CALIB', dirs)]

# loop over dirs
counter = 1
for(mydir in dirs){
    # show counter
    print(paste0(counter, '/', length(dirs)))

    # list up files
    files = list.files(mydir, pattern='.jpg')

    # generate df
    mydf = data.frame(filename = files, label='')

    # save
    write.csv(mydf, paste0(mydir, '/label.csv'), row.names=F)

    # update counter
    counter = counter + 1
}
