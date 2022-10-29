#!/usr/bin/python
import glob
import os
import sys
import cv2
import argparse
from pathlib import Path
from PIL import Image
import numpy as np
import json

class Preprocess(object):
    def __init__(self):
        self.x_width = 380
        self.y_width = 480
        self.min_x_ROI = 260
        self.min_y_ROI = 0

    def readArguments(self):
        # build argument parser
        ap = argparse.ArgumentParser()
        ap.add_argument("-a", "--avipath", type=str, help="dir housing videos")
        self.args = vars(ap.parse_args())

    def RepresentsInt(self, s):
        try:
            int(s)
            return True
        except ValueError:
            return False

    def prettifyNum(self, num):
        if len(str(num))==1:
            num_pretty = '000'+str(num)
        elif len(str(num))==2:
            num_pretty = '00'+str(num)
        elif len(str(num))==3:
            num_pretty = '0'+str(num)
        else:
            num_pretty = str(num)
        return num_pretty

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
            keyinput = input("does this look ok? (min_x_ROI="+str(self.min_x_ROI)+"). If OK, press y. Otherwise, input disired min_x_ROI value\n") #input should be raw_input for py2
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
        return self.min_x_ROI

    def selectROI_sweepsave(self, files, outdir):
        files_CALIB = [myfile for myfile in files if ('CALIB' in myfile) and not os.path.isdir('/'.join(myfile.split('/')[:-2])+'/images_mean/' + myfile.split('/')[-1].replace('.avi',''))]
        uniqs = []
        ROIs = []
        for myfile in files_CALIB:
            print('processing '+myfile+'...')
            camera = cv2.VideoCapture(myfile)
            ROIs.append(self.selectROI(camera))
            uniqs.append('_'.join(myfile.split('/')[-1].split('_')[:3]))
        if os.path.exists(outdir+'selectROIs.json'):
            os.system('rm '+outdir+'selectROIs.json')
        with open(outdir+'selectROIs.json', 'w') as outfile:
            print(dict(zip(uniqs, ROIs)))
            json.dump(dict(zip(uniqs, ROIs)), outfile)
        print('[*] selectROIs.json saved to '+outdir)

    def generate_mean(self, img, img_prev, img_prevprev):
        # calculate mean
        mybuffer_mean = np.mean([np.asarray(img), np.asarray(img_prev), np.asarray(img_prevprev)], axis=0).astype(np.uint8)

        # round values in array
        mybuffer_mean = np.array(np.round(mybuffer_mean), dtype=np.uint8)

        # return mean
        return mybuffer_mean

    def processSingleVideo(self, video, ROI):
        # create outdir
        video_split = video.split('/')
        filename = video.split('/')[-1]
        filename_split = filename.split('_')
        outdir = '/'.join(video_split[:-2])+'/images_mean/' + filename.replace('.avi','') + '/'
        if os.path.isdir(outdir):
            return
        else:
            Path(outdir).mkdir(parents=True, exist_ok=True)

        # read video
        camera = cv2.VideoCapture(video)

        # crop
        count = 0
        while True:
            (grabbed, img) = camera.read()
            if not grabbed:
                break
            if count % 5 == 0:
                img = img[self.min_y_ROI:(self.min_y_ROI+self.y_width), ROI:(ROI+self.x_width)]
                if count==0:
                    img_prev = img.copy()
                elif count==5:
                    img_prevprev = img_prev.copy()
                    img_prev = img.copy()
                else:
                    # generate mean
                    img_mean = self.generate_mean(img, img_prev, img_prevprev)
                    filename_mean = outdir + self.prettifyNum(count//5-1)+".jpg"
                    Image.fromarray(np.uint8(img_mean)).save(filename_mean, "JPEG")
                    img_prevprev = img_prev.copy()
                    img_prev = img.copy()
            count += 1

        # cleanup the camera and close any open windows
        camera.release()

    def main(self, args):
        if args['avipath'] is None:
            print('argument is missing! exiting...')
            sys.exit(1)

        # load avi files
        files = [f for f in glob.glob(args['avipath']+'/*.avi') if not (os.path.isdir('/'.join(f.split('/')[:-2])+'/images_mean/' + f.split('/')[-1].replace('.avi','')) | os.path.isfile('/'.join(f.split('/')[:-2])+'/predictions/' + f.split('/')[-1].replace('.avi','_prediction.csv')))]
        files = sorted(files)

        # select ROIs for all CALIB files
        if args['avipath'][-1] != '/':
            args['avipath'] += '/'
        if args['selectROI'] == "True":
            self.selectROI_sweepsave(files, args['avipath'])

        # load json files
        with open(args['avipath']+'selectROIs.json') as infile:
            ROIs = json.load(infile)
        print(ROIs)

        uniq_prev = ''
        count = 0
        for filename in files:
            print('processing '+filename+'... [progress: '+str(count+1)+'/'+str(len(files))+']')
            uniq = '_'.join(filename.split('/')[-1].split('_')[:3])
            if uniq not in ROIs:
                print("key not found! skipping... :"+uniq)
                continue
            self.processSingleVideo(filename, ROIs[uniq])
            uniq_prev = uniq
            count += 1

if __name__=='__main__':
    preprocess = Preprocess()
    preprocess.readArguments()
    preprocess.main(preprocess.args)
