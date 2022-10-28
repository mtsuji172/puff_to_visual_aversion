This directory contains codes for acquiring and analyzing behavioral data of the fly-on-the-ball paradigm.

1. System requirements

1-1. Data acquisition

Supports Ubuntu 20.04 amd64 operating system. This is currently our only supported platform.
No special requirements exist for CPU, RAM, Disk space/ speed. We have tested on a PC with Intel(R) Core(TM) i3-7100 CPU x 4, 8GB RAM.
For control of LED matrices, raspberry Pi 2 installed with Adafruit HAT is required. Tested only on Raspbian Lite (the following default packages were removed for efficiency: bluez bluez-firmware pi-bluetooth triggerhappy pigpio).
For control of a solenoid valve to implement air puff applications, Arduino/ Genuino Uno is required.
    
1-2. Data analysis

CPU (and accordingly RAM) usage can be relatively high due to parallel processing implementation, but the original code can be tweaked to limit the number of CPUs used. Machine-learning algorithm has only been tested on NVIDIA GeForce GTX 1660 SUPER, CUDA Version: 11.4, Driver Version: 470.141.03. 


2. Installation guide

2-1. Instructions

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

2-2. Typical install time on a "normal" desktop computer

Installation of the rpi-rgb-led-matrix software should complete within minutes.


3. Instrcutions for use / Demo

After transferring a fly to the LED arena, do the following steps to acquire data:

3-1. Execute a server file in the Raspberry Pi: sudo ./darkobject.py

3-2. On the PC, execute the main program: python repeatExecute_singleobject.py --host ip.of.raspi --genotype yourGenotype --ID IDofFly

After completing above steps, data acquisition is to proceed automatically, and in the case of repeatExecute_singleobject.py, should last for approximately 40min to complete 10 trials. Resulting data of each trial are: ball tracking csv file, video file, and json metadata file. 
