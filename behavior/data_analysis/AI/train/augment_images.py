# import the necessary packages
from keras.preprocessing.image import ImageDataGenerator
from keras.preprocessing.image import img_to_array
from keras.preprocessing.image import load_img
import numpy as np
import argparse
import os
from pathlib import Path
from tqdm import tqdm

# construct the argument parser and parse the arguments
ap = argparse.ArgumentParser()
ap.add_argument("-d", "--dataset", required=True,
	help="path to the input images")
ap.add_argument("-o", "--output", required=True,
	help="path to output directory to store augmentation examples")
ap.add_argument("-t", "--total", type=int, default=10,
	help="# of training samples to generate")
args = vars(ap.parse_args())

# create ourput dir
Path(args['output']).mkdir(parents=True, exist_ok=True)

# load the input image, convert to a NumPy array, and reshape to have an extra dimension
print("[INFO] loading example image...")
imagePaths = []
for r, d, f in os.walk(args['dataset']):
    for file in f:
        if '.jpg' in file:
            imagePaths.append(os.path.join(r,file))

for image in tqdm(imagePaths):
    image = load_img(image)
    image = img_to_array(image)
    image = np.expand_dims(image, axis=0)
    # construct the image generator for augmentation -> initialize the total number of images generated
    aug = ImageDataGenerator(
            rotation_range=10,
            zoom_range=0.1,
            width_shift_range=0.1,
            height_shift_range=0.1,
            horizontal_flip=True,
            fill_mode="nearest")
    total = 0

    # construct the actual Python generator
    imageGen = aug.flow(image, batch_size=32, save_to_dir=args["output"],
            save_prefix="image", save_format="jpg")

    # loop over examples from augmentation generator
    for image in imageGen:
            total += 1
            # if we have reached the end, break from the loop
            if total == args["total"]:
                    break
