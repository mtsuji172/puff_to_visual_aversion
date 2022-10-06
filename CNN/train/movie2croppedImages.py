#!/usr/bin/python
import glob
import os
import sys
import cv2
import argparse

class Crop(object):
    def __init__(self):
        self.readArguments()
        self.x_width = 380
        self.y_width = 480
        self.min_x_ROI = 260
        self.min_y_ROI = 0
        self.outdir = self.args['outdir']
        if self.outdir[-1]!='/':
            self.outdir += '/'

    def readArguments(self):
        # build argument parser
        ap = argparse.ArgumentParser()
        ap.add_argument("-d", "--dir", type=str, help="dir housing videos")
        ap.add_argument("-o", "--outdir", type=str, help="dir to house cropped images")
        self.args = vars(ap.parse_args())

    def prettifyNum(self, num):
        if len(str(num))==1:
            num_pretty = '00'+str(num)
        elif len(str(num))==2:
            num_pretty = '0'+str(num)
        else:
            num_pretty = str(num)
        return num_pretty

    def RepresentsInt(self, s):
        try:
            int(s)
            return True
        except ValueError:
            return False

    def selectROI(self, camera):
        (grabbed, img) = camera.read()
        if img is None:
            sys.exit("No input image") #good practice

        # write ROI
        cv2.rectangle(img, (self.min_x_ROI, self.min_y_ROI), (self.min_x_ROI+self.x_width, self.min_y_ROI+self.y_width), (0,255,0), 2)
        cv2.imshow('img',img)
        cv2.waitKey()
        cv2.destroyAllWindows()
        while True:
            keyinput = input("does this look ok? (min_x_ROI="+str(self.min_x_ROI)+"). If OK, press y. Otherwise, input desired min_x_ROI value\n") #input should be raw_input for py2
            if keyinput == "y":
                cv2.destroyAllWindows()
                break
            elif self.RepresentsInt(keyinput):
                self.min_x_ROI = int(keyinput)
                (grabbed, img) = camera.read()
                cv2.rectangle(img, (self.min_x_ROI, self.min_y_ROI), (self.min_x_ROI+self.x_width, self.min_y_ROI+self.y_width), (0,255,0), 2)
                cv2.imshow('img',img)
                cv2.waitKey()
                cv2.destroyAllWindows()
            else:
                print("[ERROR] keyinput must be either 'y' or integer for nuew min_x_ROI!\n")
        print("min_x_ROI determined to be: "+str(self.min_x_ROI))
        return

    def processSingleVideo(self, video, ask):
        # create output dir
        mydir = self.outdir + video.split('/')[-1].replace('.avi','')
        if os.path.isdir(mydir):
            print('already precessed (outdir exists). skipping...')
            return
        os.system('mkdir '+mydir)

        # read video
        camera = cv2.VideoCapture(video)

        # select appropriate ROI
        if ask==True:
            self.selectROI(camera)

        # crop
        count = 0
        while True:
            (grabbed, img) = camera.read()
            if not grabbed:
                break
            if count % 5 == 0:
                img = img[self.min_y_ROI:self.min_y_ROI+self.y_width, self.min_x_ROI:self.min_x_ROI+self.x_width]
                cv2.imwrite(mydir+'/'+mydir.split('/')[-1]+'_'+self.prettifyNum(int(count/5))+".jpg", img)
            count += 1
        # cleanup the camera and close any open windows
        camera.release()

    def main(self):
        files = [f for f in glob.glob(self.args['dir']+'/*.avi')]
        files = sorted(files)
        uniq_prev = ''
        count = 0
        for filename in files:
            print('processing '+filename+'... [progress: '+str(count)+'/'+str(len(files))+']')
            uniq = '_'.join(filename.split('/')[-1].split('_')[:3])
            if uniq != uniq_prev:
                self.processSingleVideo(filename, ask=True)
            else:
                self.processSingleVideo(filename, ask=False)
            uniq_prev = uniq
            count += 1

if __name__=='__main__':
    Crop().main()
