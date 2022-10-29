#!/usr/bin/python

# import the necessary packages
from keras.models import Sequential
from keras.layers.convolutional import Conv2D
from keras.layers.convolutional import MaxPooling2D
from keras.layers.core import Activation
from keras.layers.core import Flatten
from keras.layers.core import Dense
from keras.layers.core import Dropout
from keras.optimizers import Adam
from sklearn.preprocessing import LabelBinarizer
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report
from PIL import Image
from imutils import paths
import numpy as np
import argparse
import os
import matplotlib.pyplot as plt
import pickle
from progress.bar import IncrementalBar
import tensorflow as tf
from tensorflow.compat.v1 import ConfigProto
from tensorflow.compat.v1 import InteractiveSession
config = ConfigProto()
config.gpu_options.allow_growth = True
session = InteractiveSession(config=config)

# construct the argument parser and parse the arguments
ap = argparse.ArgumentParser()
ap.add_argument("-d", "--dataset", type=str, default="image", help="path to directory containing the image dataset")
args = vars(ap.parse_args())

# params
EPOCHS = 500
WIDTH = 119
HEIGHT = 150
BS = 64

def dirSlashHandler(mystring):
    if mystring[-1] != '/':
        mystring += '/'
    return mystring

# grab all image paths in the input dir, then initialize the list of images and labels
print("[INFO] loading images...")
imagePaths = []
for r, d, f in os.walk(args['dataset']):
    for file in f:
        if '.jpg' in file:
            imagePaths.append(os.path.join(r,file))
bar = IncrementalBar('Countdown', max = len(imagePaths))
data = []
labels = []

# loop over our input images
for imagePath in imagePaths:
    # load image, resize, scale the pixel intensities to [0, 1], and update the images list
    image = Image.open(imagePath)
    image = np.array(image.resize((WIDTH, HEIGHT))) / 255.0
    data.append(image)

    # extract the class label and update the labels list
    label = imagePath.split(os.path.sep)[-2]
    labels.append(label)
    bar.next()
bar.finish()
labelNum = len(list(set(labels)))

# encode the labels, converting them from strings to integers
lb = LabelBinarizer()
labels = lb.fit_transform(labels)

# show each of the possible class labels
for (i, label) in enumerate(lb.classes_):
	print("{}. {}".format(i + 1, label))

# split the dataset into a training and testing set
(trainX, testX, trainY, testY) = train_test_split(np.array(data), np.array(labels), test_size=0.20)

# define CNN architecture
model = Sequential()
model.add(Conv2D(8, (3, 3), padding="same", input_shape=(HEIGHT, WIDTH, 3)))
model.add(Activation("relu"))
model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
model.add(Conv2D(16, (3, 3), padding="same"))
model.add(Activation("relu"))
model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
model.add(Conv2D(32, (3, 3), padding="same"))
model.add(Activation("relu"))
model.add(Dropout(0.3))
model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
model.add(Conv2D(64, (3, 3), padding="same"))
model.add(Activation("relu"))
model.add(Dropout(0.3))
model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
model.add(Conv2D(128, (3, 3), padding="same"))
model.add(Activation("relu"))
model.add(Dropout(0.3))
model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
model.add(Conv2D(256, (3, 3), padding="same"))
model.add(Activation("relu"))
model.add(Dropout(0.3))
model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
model.add(Flatten())
model.add(Dense(2048))
model.add(Activation("relu"))
model.add(Dropout(0.3))
model.add(Dense(256))
model.add(Activation("relu"))
model.add(Dropout(0.3))
model.add(Dense(64))
model.add(Activation("relu"))
model.add(Dropout(0.3))
model.add(Dense(labelNum))
model.add(Activation("softmax"))

model.summary()

# train using the Adam optimizer
print("[INFO] training network...")
opt = Adam(lr=1e-3, decay=1e-3 / EPOCHS)
model.compile(loss="categorical_crossentropy", optimizer=opt,
	metrics=["accuracy"])

# construct the image generator for data augmentation
H = model.fit(trainX, trainY, validation_data=(testX, testY), epochs=EPOCHS, batch_size=BS)

# evaluate the network
print("[INFO] evaluating network...")
predictions = model.predict(testX, batch_size=BS)
print(classification_report(testY.argmax(axis=1),
	predictions.argmax(axis=1), target_names=lb.classes_))

# serialize the model to disk
print("[INFO] saving the model...")
model.save('mymodel.model')

# save the label binarizer to disk
print("[INFO] serializing label binarizer...")
f = open('lb.pickle', "wb")
f.write(pickle.dumps(lb))
f.close()

# accuray and loss plot
# plot the training loss and accuracy
plt.style.use("ggplot")
plt.figure()
N = EPOCHS
plt.plot(np.arange(0, N), H.history["loss"], label="train_loss")
plt.plot(np.arange(0, N), H.history["val_loss"], label="val_loss")
plt.plot(np.arange(0, N), H.history["accuracy"], label="train_accuracy")
plt.plot(np.arange(0, N), H.history["val_accuracy"], label="val_accuracy")
plt.title("Training Loss and Accuracy")
plt.xlabel("Epoch #")
plt.ylabel("Loss/Accuracy")
plt.legend(loc="upper left")
plt.savefig('performanceDynamics.png')
