#!/usr/bin/python

import cv2
import glob
import argparse
import os
from tqdm import tqdm
import sys
import json

######################################
# params
######################################
# argparse
ap = argparse.ArgumentParser()
ap.add_argument('-d', '--dir', type=str, help='path to dir containing "avi" dir')
args = vars(ap.parse_args())

info_list = ['DATE','GENOTYPE','DPE','ID','TRIAL']

# output location
outdir = args['dir'] + 'metadata/'
if not os.path.exists(outdir):
    os.makedirs(outdir)

######################################
# functions
######################################

def get_meta(filename):
    global info_list

    # get filename w/o path
    outname = filename.split('/')[-1].replace('.avi','.json')

    # Load a video
    cap = cv2.VideoCapture(filename)

    # get total frame num
    fnum = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
    out = {'fnum':[fnum]}

    # get other metadata from filename
    outname_split = outname.replace('.json','').split('_')

    for info in info_list:
        myinfo = [x.replace(info, '') for x in outname_split if info in x]
        out = {**out, **{info:myinfo}}

    # save
    out_json = json.dumps(out)
    with open(outdir+outname, 'w') as outfile:
        outfile.write(out_json)

    return

######################################
# main
######################################
# list files
myfiles = sorted(glob.glob(args['dir']+'avi/*.avi'))

# loop over files
for myfile in myfiles:
    if not os.path.exists(outdir+myfile.split('/')[-1].replace('.avi','.csv')):
        get_meta(myfile)
