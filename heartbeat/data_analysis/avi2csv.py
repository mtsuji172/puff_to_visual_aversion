#!/usr/bin/python

import cv2
import time
import imutils
import pandas as pd
import glob
import os
import argparse
import numpy as np
import multiprocessing
from pathlib import Path
import json

#############################
# params
#############################
ap = argparse.ArgumentParser()
ap.add_argument('-d', '--dir', type=str, help='path to dir containing videos')
args = vars(ap.parse_args())

# resizing of frame
resizewidth = 52 # original x 1/2
resizeheight = 25 # original x 1/2

# blur size
blursize = (7,7)

#############################
# functions
#############################

def track(myvideo):

    # get video
    camera = cv2.VideoCapture(myvideo)

    # loop over images
    counter = 0
    ID = 0
    while True:
        # grab the current frame
        (grabbed, frame) = camera.read()

        # if the frame could not be grabbed, break 
        if not grabbed:
            break

        # convert frame to grayscale, and blur it
        frame = imutils.resize(frame, width=resizewidth, height=resizeheight)
        frame_gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
        frame_gray = cv2.GaussianBlur(frame_gray, blursize, 0)

        # record
        df_t = pd.DataFrame({str(counter): np.asarray(frame_gray).flatten()}).transpose()

        if counter == 0:
            myrecord = df_t
        else:
            myrecord = pd.concat([myrecord, df_t])

        # update
        counter += 1

        # if the `q` key is pressed, break from the lop
        key = cv2.waitKey(1) & 0xFF
        if key == ord("q"):
            break

    # cleanup the camera and close any open windows
    cv2.destroyAllWindows()

    # remove the last line (recording is often interrupted during the scanning of the last frame -> the last frame is often only a partial image)
    myrecord = myrecord.head(myrecord.shape[0] -1)

    # output a file
    myrecord.to_csv(myvideo.replace('.avi','.csv'))

    return


#############################
# main
#############################
# list files
myfiles = glob.glob(args['dir']+'avi/*.avi')
myfiles = [myfile for myfile in myfiles if "label" not in myfile]

# list files that have been processed already
myfiles_csv = glob.glob(args['dir']+'Pixels/*.csv')
myfiles_processed = [myfile.split('/')[-1].replace('.csv','.avi') for myfile in myfiles_csv]

# select files not processed yet
myfiles = [myfile for myfile in myfiles if myfile.split('/')[-1] not in myfiles_processed]
print(myfiles)

# execute "track"
pool = multiprocessing.Pool(processes=6)
pool.map(track, myfiles)

# transfer files to respective dirs
Path(args['dir']+"Pixels/").mkdir(parents=True, exist_ok=True)
os.system("mv "+args['dir']+"avi/*.csv "+args['dir']+"Pixels/")
