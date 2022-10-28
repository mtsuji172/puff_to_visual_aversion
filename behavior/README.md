This directory contains codes for acquiring and analyzing behavioral data of the fly-on-the-ball paradigm.

1. System requirements
1-1. Data acquisition
Supports Ubuntu 20.04 amd64 operating system. This is currently our only supported platform.
No special requirements exist for CPU, RAM, Disk space/ speed. We have tested on a PC with X GHz CPU x 4, 8GB RAM, kb/s Disk speed.
For control of LED matrices, raspberry Pi 2 installed with Adafruit HAT is required. Tested only on Raspbian Lite (the following default packages were removed for efficiency: bluez bluez-firmware pi-bluetooth triggerhappy pigpio).
For control of a solenoid valve to implement air puff applications, Arduino/ Genuino Uno is required.
    
As for data analysis, CPU (and accordingly RAM) usage can be relatively high due to parallel processing implementation, but the original code can be tweaked to limit the number of CPUs used. Machine-learning algorithm has only been tested on NVIDIA GeForce GTX 1660 SUPER, CUDA Version: 11.4, Driver Version: 470.141.03. 
