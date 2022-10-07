#!/usr/bin/python

import cv2, time, imutils, time, csv, glob, os, argparse

# construct parser 
ap = argparse.ArgumentParser()
ap.add_argument("-f", "--filename", type=str, default='myrecord.csv', help="filename")
ap.add_argument("-r", "--reclength", type=int, default=30, help="recording length in sec")
args = vars(ap.parse_args())

# constants
reclength = args['reclength']

# Read frame
print '[*] recording start... %d sec' % reclength
camera = cv2.VideoCapture(0)
fourcc = cv2.VideoWriter_fourcc(*'XVID')
fps = 47.0
out = cv2.VideoWriter('../data/'+args['filename']+'.avi', fourcc, fps, (640,480))

# loop over the frames of the video
t_start = time.time()
while True:
    t_now = time.time() - t_start
    if t_now >= reclength:
        break

    # grab the current frame
    grabbed, frame = camera.read()

    if grabbed == True:
        # resize the frame, show and record 
        frame = imutils.resize(frame, width=640)
        cv2.imshow("frame", frame)
        out.write(frame)

# cleanup the camera and close any open windows
camera.release()
out.release()
cv2.destroyAllWindows()
