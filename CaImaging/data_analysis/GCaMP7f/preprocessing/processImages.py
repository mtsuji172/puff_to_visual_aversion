#!/usr/bin/python3
# python processImages.py path/to/dir/containing/image/sequences/ 

from alignImages import AlignImages
import os
import sys
from pathlib import Path
import glob

path2parentdir = sys.argv[1]
dirs = next(os.walk(path2parentdir))[1]
count = 0
for mydir in dirs:
    print('processing '+mydir+'... (' + str(count+1)+'/'+str(len(dirs))+')')
    path2dir = path2parentdir + mydir

    # if GminusR already exists, skip
    if os.path.isdir(path2dir+"/GminusR"):
        print(path2dir+"/GminusR"+" already processed! Skipped.")
        count += 1
        continue

    # prep dirs
    print('prepping dirs for files of different channels...')
    Path(path2dir+"/chanA").mkdir(parents=True, exist_ok=True)
    Path(path2dir+"/chanB").mkdir(parents=True, exist_ok=True)

    # move relevant files
    print('sorting files of different channels...')
    for file in glob.glob(path2dir+'/*.tif'):
        if "ChanA" in file:
            os.system("mv "+file+" "+path2dir+"/chanA/")
        elif "ChanB" in file:
            os.system("mv "+file+" "+path2dir+"/chanB/")

    # execute GminusR.ijm
    _, _, files = next(os.walk(path2dir+'/chanA/'))
    fileNo = len(files)
    os.system("~/bin/fiji/ImageJ-linux64 -macro GminusR.ijm '"+path2dir+"/ "+str(fileNo)+"'")
    Path(path2dir+"/GminusR").mkdir(parents=True, exist_ok=True)
    os.system("mv "+path2dir+"/*.tif "+path2dir+"/GminusR/")

    # align maxprojected images
    print('aligning images...')
    AlignImages().main(path2dir+'/GminusR/', 'ChanA_0001_0001_0001_0001.tif')
    count += 1
