This directory contains codes for acquiring and analyzing behavioral data of the fly-on-the-ball paradigm.

<b>1. System requirements</b>

<b>1-1. Data acquisition</b>

Supports Ubuntu 20.04 amd64 operating system. This is currently our only supported platform.
No special requirements exist for CPU, RAM, Disk space/ speed. We have tested on a PC with Intel(R) Core(TM) i3-7100 CPU x 4, 8GB RAM.
For control of LED matrices, raspberry Pi 2 installed with Adafruit HAT is required. Tested only on Raspbian Lite (the following default packages were removed for efficiency: bluez bluez-firmware pi-bluetooth triggerhappy pigpio).
For control of a solenoid valve to implement air puff applications, Arduino/ Genuino Uno is required.
    
<b>1-2. Data analysis</b>

CPU (and accordingly RAM) usage can be relatively high due to parallel processing implementation, but the original code can be tweaked to limit the number of CPUs used. Machine-learning algorithm has only been tested on NVIDIA GeForce GTX 1660 SUPER, CUDA Version: 11.4, Driver Version: 470.141.03. 


<b>2. Installation guide</b>

<b>2-1. Instructions</b>

Softwares

Install rpi-rgb-led-matrix software (https://github.com/hzeller/rpi-rgb-led-matrix) to Raspberry Pi with the following parameters:
 led-rows=16 ;
 led-cols=32 ;
 led-chain=6 ;
 led-gpio-mapping=adafruit-hat-pwm ;
 led-pwm-bits=4 ;
 led-pwm-lsb-nanoseconds=80.
 
Please refer to the manufacturer's installation guide to install Arduino IDE (v.1.8.19).

Cameras

We use python opencv to access the camera. Currenty only MSP-3080 (Panrico) is supported, but any webcam with similar spec should work fine as long as they are accessible by python opencv. 

LED matrices

We use Medium 16x32 RGB LED matrix panel 420 (Adafruit) for visual stimulation. Hardware and software configurations were based on rpi-rgb-led-matrix software (https://github.com/hzeller/rpi-rgb-led-matrix).

<b>2-2. Typical install time on a "normal" desktop computer</b>

Installation of the rpi-rgb-led-matrix software should complete within minutes.


<b>3. Instrcutions for use / Demo</b>

After transferring a fly to the LED arena, do the following steps to acquire data (files are contained in ther "./data_acquisition" directory):

Execute a server file (in "./data_acquisition/server/" directory) in the Raspberry Pi: sudo ./darkobject.py

On the PC, execute the main program (in "./data_acquisition/client/"): python repeatExecute_singleobject.py --host ip.of.raspi --genotype yourGenotype --ID IDofFly

After completing above steps, data acquisition is to proceed automatically, and in the case of repeatExecute_singleobject.py, should last for approximately 40min to complete 10 trials. Resulting data of each trial are: ball tracking csv file, video file, and json metadata file. 

<b>3-1. AI-aided identification of behaviors</b>

First we need to prepare a training data set. This can be achieved by the following steps (files are contained in "./data_analysis/AI/train/"):

(i) Convert avi files into cropped jpg images, like so:
python movie2croppedImages.py -d path/to/dir/containing/avi/files/

This program prompts you to manually set ROI for each video as it proceeds.

(ii) Generate "average" images of three consecutive images, like so:
python generate_mean.py -d path/to/dir/containing/images/

(iii) Manually label each of the average images, and save the labels as "label.csv" in the corresponding dir containing average images. 

(iv) Sort images into directories of each behavioral label, like so:
python sort_images.py

(v) augment images like so:
python augment_images.py -d path/to/images/ -o path/to/save/augmented/images/ -t "# of training samples to generate"

Then we train the model, like so:
python train_model.py -d path/to/dir/containing/training/dataset/

It takes hours but probably not days to complete, depending on your training data set and your PC. The result is saved as a mymodel.model file (the model per se) and a lb.pickle file (label binarizer).

(files used in the following are contained in "./data_analysis/AI/")
Before utilizing the trained model, we must first convert each video file to cropped "average" image sequences as we did to prepare a training data set. This can be conveniently done like so:
python preprocess.py -a path/to/dir/containing/videos/

Now we are ready to apply our trained model to label each frame, like so:
python -i path/to/average/images/ -m path/to/trained/model/ -l path/to/label/binarizer/ -p path/to/output/dir/

The resultant frame-wise predictions are stored in path/to/output/dir/ as a csv file per video. These data can now be analyzed to obtain the overall frequency of each behavior per video, like so:
python statistics.py -p path/to/dir/containing/frame-wise/predictions/ -o path/to/output/dir/


<b>3-2. all the other analyses (ball tracking, integration with AI-identified behaviors)</b> (files are contained in "./data_analysis/others/" directory)
Open up main_CS.R (or other main_~.R files) with your favorite editor and execute line-by-line. This program analyzes the ball tracking results and integrates those results with the behavior labeling results.


Note: in case a pair of directories/ files exist with identical names except that one additionally contains "\_LEDfreq", the one with "\_LEDfreq" is the modified version of the original for experiments in which varying LED frequencies are to be tested.
