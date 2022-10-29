#!/usr/bin/python3
# python processImages.py ../Tk-GCaMP6s/

import os
import sys
import glob

target = 'GminusR/aligned/'

path2parentdir = sys.argv[1]
dirs = next(os.walk(path2parentdir))[1]
count = 1
for mydir in dirs:
    print('processing '+mydir+'... (' + str(count)+'/'+str(len(dirs))+')')
    path2dir = path2parentdir + '/' + mydir
    path2subdir = path2dir + '/' + target

    # if Results.csv already exists, skip
    if os.path.isfile(path2subdir+"Results.csv"):
        print(path2subdir+"Results.csv already exists! Skipped.")
        count += 1
        continue
    if not os.path.isdir(path2subdir):
        print(path2subdir)
        print("path not exist! skipping...")
        count += 1
        continue
    _, _, files = next(os.walk(path2subdir))
    fileNo = len([x for x in files if '.tif' in x])
    print("~/bin/fiji/ImageJ-linux64 -macro multimeasure.ijm '"+str(fileNo)+" "+path2dir+"/ "+target+"'")
    os.system("~/bin/fiji/ImageJ-linux64 -macro multimeasure.ijm '"+str(fileNo)+" "+path2dir+"/ "+target+"'")
    count += 1
