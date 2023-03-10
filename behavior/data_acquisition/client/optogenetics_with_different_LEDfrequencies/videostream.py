#!/usr/bin/python
import cv2, time, imutils, time, csv, glob, os

### Read data
camera = cv2.VideoCapture(0)

### initialize the previous/ initial frame in the video stream
initialFrame = None

### loop over the frames of the video
while True:
    ### grab the current frame
    (grabbed, frame) = camera.read()
 
    ### resize the frame, convert it to grayscale, and blur it
    frame = imutils.resize(frame, width=500)
 
    ### if the initial frame is None, initialize it
    if initialFrame is None:
        initialFrame = frame
        continue
    
    ### show the frame
    cv2.imshow("frame", frame)

    ### if the `q` key is pressed, break from the lop
    key = cv2.waitKey(1) & 0xFF
    if key == ord("q"):
        break
 
### cleanup the camera and close any open windows
camera.release()
cv2.destroyAllWindows()
