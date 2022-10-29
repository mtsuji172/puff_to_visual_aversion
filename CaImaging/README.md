This directory contains codes for acquiring and analyzing Calcium imaging data of the adult fly brain.

1. System requirements

1-1. Data acquisition

Supports Windows 10 operating system. This is currently our only supported platform.
No special requirements exist for CPU, RAM, Disk space/ speed. We have tested on a PC with XXX.
For control of LED matrices, Raspberry Pi 2 installed with Adafruit HAT is required. Users may solder a wire between GPIO 4 and 18 to avoid flickering. Tested only on Raspbian Lite. Raspberry Pi 2 needs to have rpi-rgb-led-matrix software (https://github.com/hzeller/rpi-rgb-led-matrix) installed. In addition, the following packages that come with the OS by default may be removed for efficiency: bluez bluez-firmware pi-bluetooth triggerhappy pigpio.
For control of a solenoid valve to implement air puff applications, Arduino/ Genuino Uno is required.
For acquisition of the Calcium images, ThorImageLS (v3.2.2018.4241) and MaiTai (v0250-2.00.23) is required to control the Thorlabs Bergamo two-photon microscope and Mai Tai 2 laser. 
    
1-2. Data analysis

CPU (and accordingly RAM) usage can be relatively high due to parallel processing implementation, but the original code can be tweaked to limit the number of CPUs used. Requires Fiji (v2.3.0) for multi-ROI GCaMP quantification.



2. Installation guide

2-1. Instructions

Softwares
Please refer to the manufacturer's installation guide to install ThorImage LS (v3.2.2018.4241), MaiTai (v0250-2.00.23), and Arduino IDE (v.1.8.19).


LED matrices

We use Medium 16x32 RGB LED matrix panel 420 (Adafruit) for visual stimulation. Hardware and software configurations were based on rpi-rgb-led-matrix software (https://github.com/hzeller/rpi-rgb-led-matrix).


2-2. Typical install time on a "normal" desktop computer

Installation of the rpi-rgb-led-matrix software should complete within minutes.


3. Instrcutions for use / Demo

First, execute a server program in the Raspberry Pi by: sudo ./darkobject.py. This program waits for a key input: if you press "s", the program sends a TTL signal to Bergamo 2P microscope to initiate the imaging (and to arduino for puff application when appropriate); if you press "t", the program start sending a TTL signal to arduino to initiate applying air puffs so that you can adjust the puff-delivering glass pipette position/angle before recording. We recommend you set the air pressure minimum while adjusting to prevent the fly from desensitizing to puffs; if you press "q", the program quits.  

After completing above steps, data acquisition is to proceed automatically and should last for 80s to complete a single trial. Resulting data is an image sequence. 
