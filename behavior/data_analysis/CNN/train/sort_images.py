#!/usr/bin/python

"""
this code assumes that frame-by-frame manual labeling is saved as "label.csv" in each mean_images directory

This version assumes that all images are named like so: "0001.jpg"
"""

import os
import pandas as pd
from tqdm import tqdm
from pathlib import Path

# params
parent_dir = 'path/to/dir/containing/images_mean/dir/for/each/movie/'
out_dir = 'path/to/out/dir/'

# list up child dirs
child_dirs = os.listdir(parent_dir)

# loop over child dirs
for k in tqdm(range(len(child_dirs))):
    child_dir = child_dirs[k]
    print(child_dir)
    if not os.path.isdir(child_dir):
        continue

    # define filename_prefix
    filename_prefix = child_dir.split('/')[-1]

    # get label.csv
    label = pd.read_csv(child_dir+'/label.csv')

    # loop over rows
    # if labeled, copy the file to out_dir
    for r in range(len(label)):
        if pd.isnull(label['label'][r]):
            continue
        filename = label['filename'][r]
        Path(out_dir+'/'+str(label['label'][r])).mkdir(parents=True, exist_ok=True)
        filename_full = parent_dir + '/' + child_dir + '/' + filename
        os.system('cp '+filename_full+' '+out_dir+'/'+str(label['label'][r])+'/'+filename_prefix+'_'+filename)
