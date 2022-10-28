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
