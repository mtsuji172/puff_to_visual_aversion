#!/usr/bin/python3

# import the necessary packages
import numpy as np
import argparse
from PIL import Image
import os
from progress.bar import IncrementalBar
import pandas as pd
from pathlib import Path
import multiprocessing

# construct the argument parser and parse the arguments
ap = argparse.ArgumentParser()
ap.add_argument("-d", "--dataset", required=True,
	help="path to input directory of images")
args = vars(ap.parse_args())

def prettifyNum(num):
    if len(str(num))==1:
        num_pretty = '00'+str(num)
    elif len(str(num))==2:
        num_pretty = '0'+str(num)
    else:
        num_pretty = str(num)
    return num_pretty

def generate_mean(imagePath):
    # if outfile already exists, skip
    imagePath_split = imagePath.split('/')
    filename = imagePath.split('/')[-1]
    filename_split = filename.split('_')
    mean_grandparentdir = '/'.join(imagePath_split[:-3])+'/'+ imagePath_split[-3]+'_mean/'
    mean_parentdir = '/'.join(imagePath_split[:-3])+'/'+ imagePath_split[-3]+'_mean/' + imagePath_split[-2] + '/'
    filename_mean = mean_parentdir + filename
    if os.path.isfile(filename_mean):
        #bar.next()
        #continue
        return

    # list image of neighboring frames
    imagePath_neighbor = '/'.join(imagePath.split('/')[:-1]) + '/'
    filename_prev = '_'.join(filename_split[:-1])+'_'+prettifyNum(int(filename_split[-1].replace('.jpg',''))-1)+'.jpg'
    filename_next = '_'.join(filename_split[:-1])+'_'+prettifyNum(int(filename_split[-1].replace('.jpg',''))+1)+'.jpg'

    if (not os.path.isfile(imagePath_neighbor+filename_prev)) or (not os.path.isfile(imagePath_neighbor+filename_next)):
        return

    # load images 
    image = Image.open(imagePath)
    image_prev = Image.open(imagePath_neighbor+filename_prev)
    image_next = Image.open(imagePath_neighbor+filename_next)

    # convert to numpy array
    mybuffer = np.asarray(image)
    mybuffer_prev = np.asarray(image_prev)
    mybuffer_next = np.asarray(image_next)

    # calculate mean
    w,h = image.size
    mybuffer_mean = np.zeros((h,w,3), np.float)
    for im in [image, image_prev, image_next]:
        mybuffer_mean += np.array(im, dtype=np.float)/3

    # round values in array
    mybuffer_mean = np.array(np.round(mybuffer_mean), dtype=np.uint8)

    # generate image
    image_mean = Image.fromarray(mybuffer_mean, mode='RGB')

    # save mean
    Path(mean_parentdir).mkdir(parents=True, exist_ok=True)
    Image.fromarray(np.uint8(image_mean)).save(filename_mean, "JPEG")

# grab all image paths in the input dataset directory
print('loading images..')
imagePaths = []
for r, d, f in os.walk(args['dataset']):
    for file in f:
        if '.jpg' in file:
            imagePaths.append(os.path.join(r,file))

print('generating mean images...')
pool = multiprocessing.Pool()
pool.map(generate_mean, imagePaths)
