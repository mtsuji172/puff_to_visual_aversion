#!/usr/bin/python

# import the necessary packages
from keras.models import load_model
from imutils import paths
import numpy as np
import argparse
from PIL import Image
import os
import pandas as pd
import pickle
from get_metadata import get_metadata
import multiprocessing
from pathlib import Path
import tensorflow as tf
config = tf.compat.v1.ConfigProto()
config.gpu_options.allow_growth = True
tf.compat.v1.keras.backend.set_session(tf.compat.v1.Session(config=config))

# construct the argument parser and parse the arguments
ap = argparse.ArgumentParser()
ap.add_argument("-i", "--images", required=True,
	help="path to out input directory of images")
ap.add_argument("-m", "--model", required=True,
	help="path to pre-trained model")
ap.add_argument("-l", "--labelbin", required=True,
	help="path to label binarizer")
ap.add_argument("-p", "--predictions", type=str,
    help="output dir")
args = vars(ap.parse_args())

# params
WIDTH = 119
HEIGHT = 150
confidenceThresh = 0.7

# create out dir
if args['predictions'][-1]!='/':
    args['predictions'] += '/'
Path(args['predictions']).mkdir(parents=True, exist_ok=True)

# load the pre-trained network
print("[INFO] loading pre-trained network...")
model = load_model(args["model"])
model.summary()
lb = pickle.loads(open(args["labelbin"], "rb").read())

# grab all image paths in the input images directory, then initialize
# our list of images and corresponding class labels
alldirs = os.listdir(path=args["images"])
predfiles = [f.replace('_prediction.csv','') for f in os.listdir(args['predictions']) if os.path.isfile(os.path.join(args['predictions'], f)) & ('_prediction.csv' in f)]
dirs = [mydir for mydir in alldirs if mydir not in predfiles]
count = 0
for mydir in dirs:
    print('[INFO] processing '+mydir+' ['+str(count+1)+'/'+str(len(dirs))+']...')
    outname = args['predictions']+mydir+'_prediction.csv'
    print("[INFO] loading images...")
    print(args['images']+mydir)
    imagePaths = paths.list_images(args['images']+mydir)
    data = []
    filenames = []
    # loop over our input images
    for imagePath in sorted(list(imagePaths)):
        # load the input image from disk, resize it, scale
        # the pixel intensities to the range [0, 1], and then update our
        # images list
        image = Image.open(imagePath)
        image = np.array(image.resize((WIDTH, HEIGHT))) / 255.0
        data.append(image)

        # extract the class label from the file path and update the
        # labels list
        filename = imagePath.split(os.path.sep)[-1]
        filenames.append(filename)

    print("[INFO] classifying image...")
    results = model.predict(np.array(data))

    # output the final verdicts
    results_final = []
    for i in range(len(results)):
        if np.amax(results[i]) > confidenceThresh:
            results_final.append(np.where(results[i]==np.amax(results[i]))[0])
        else:
            results_final.append([999]) #<- meaning "unknown"

    results_final_label = []
    for (i,j) in enumerate(results_final):
        if j != [999]:
            results_final_label.append(lb.classes_[j])
        else:
            results_final_label.append('NA')
    print('successful labellling rate was '+str(len(results_final_label)-results_final_label.count('NA'))+'/'+str(len(results_final_label)))
    results_final_label = pd.DataFrame.transpose(pd.DataFrame([results_final_label, filenames]))
    np.savetxt(outname, results_final_label, delimiter=",", fmt='%s')
    print('[*] result saved to '+outname)
    count += 1

# get metadata for each prediction csv
print('\nadding metadata to each predictions...')
if args['predictions'][-1] != '/':
    args['predictions'] += '/'
csvfilenames = []
for r, d, f in os.walk(args['predictions']):
    for myfile in f:
        if ('.csv' in myfile) & (not 'labeled' in myfile):
            csvfilenames.append(os.path.join(r,myfile))
for csvfilename in csvfilenames:
    get_metadata(csvfilename)
