This directory contains codes for acquiring and analyzing Calcium imaging data of the adult fly brain.

<b>1. System requirements</b>

<b>1-1. Data acquisition</b>

Supports Windows 10 operating system. This is currently our only supported platform.
No special requirements exist for CPU, RAM, Disk space/ speed.
For control of LED matrices, Raspberry Pi 2 installed with Adafruit HAT is required. Users may solder a wire between GPIO 4 and 18 to avoid flickering. Tested only on Raspbian Lite. Raspberry Pi 2 needs to have rpi-rgb-led-matrix software (https://github.com/hzeller/rpi-rgb-led-matrix) installed. In addition, the following packages that come with the OS by default may be removed for efficiency: bluez bluez-firmware pi-bluetooth triggerhappy pigpio.
For control of a solenoid valve to implement air puff applications, Arduino/ Genuino Uno is required.
For acquisition of the Calcium images, ThorImageLS (v3.2.2018.4241) and MaiTai (v0250-2.00.23) is required to control the Thorlabs Bergamo two-photon microscope and Mai Tai 2 laser. 
    
<b>1-2. Data analysis</b>

CPU (and accordingly RAM) usage can be relatively high due to parallel processing implementation, but the original code can be tweaked to limit the number of CPUs used. Requires Fiji (v2.3.0) for multi-ROI GCaMP quantification.



<b>2. Installation guide</b>

<b>2-1. Instructions</b>

Softwares
Please refer to the manufacturer's installation guide to install ThorImage LS (v3.2.2018.4241), MaiTai (v0250-2.00.23), and Arduino IDE (v.1.8.19).


LED matrices

We use Medium 16x32 RGB LED matrix panel 420 (Adafruit) for visual stimulation. Hardware and software configurations were based on rpi-rgb-led-matrix software (https://github.com/hzeller/rpi-rgb-led-matrix).


<b>2-2. Typical install time on a "normal" desktop computer</b>

Installation of the rpi-rgb-led-matrix software should complete within minutes.


<b>3. Instrcutions for use / Demo</b>

<b>3-1. Data acquisition</b>
First, execute a server program (in "./data_acquisition/" directory) in the Raspberry Pi by:
sudo ./server.py

This program waits for a key input: if you press "s", the program sends a TTL signal to Bergamo 2P microscope to initiate the imaging (and to arduino for puff application when appropriate); if you press "t", the program start sending a TTL signal to arduino to initiate applying air puffs so that you can adjust the puff-delivering glass pipette position/angle before recording. We recommend you set the air pressure minimum while adjusting to prevent the fly from desensitizing to puffs; if you press "q", the program quits.  

After completing above steps, data acquisition is to proceed automatically and should last for 80s to complete a single trial. Resulting data is a folder containing sequential images. 

These sequential images can then be passed to analysis codes.

<b>3-2. Data analysis</b>

(preprocessing: files are stored in "./data_analysis/GCaMP7f(or6s)/preprocessing/")
Assuming that the data are two-channel images of green (GCaMP signal) and red (autofluorescence of the tissue or bleed-through of the blue light from LED matrices) if the fly is not expressing RFP or its variants), we will first subtract noise (red) from GCaMP signal (green) frame by frame. We will also have to address the issue of the brain tissue moving slightly from time to time. Both of these can be achieved by running:
python processImages.py path/to/dir/containing/image/sequences/

This results in, for each directory containing sequential images of a single trial, the original images sorted into "chanA" (corresponding to green) and "chanB" (corresponding to red) directories. This also results in "GminusR" directory containing "original" and "aligned" subdirectories. "original" directory stores images in which red signal was subtracted from green signal pixel-wise, in the subdirectory of a directory housing the original images. "aligned" directory stores images in which spontaneous movements of the brain tissue is corrected.

Next we will select ROIs for signal quantification. This can be achieved in Fiji by "ROI Manager" accessible through "Analyze>tools>ROI Manager". Just open an image sequence and fire up the ROI Manager. For each cell of interest, draw a manual curve to encircle a cell, and register. Keep doing this until you registered all the cells, and save the ROI set as .RoiSet in the directory of the corresponding trial (the parent directory containing "chanA", "chanB", "GminusR").

Finally, we will quantify the total signal intensities in each ROI, of all the trials in batch, by running:
python multimeasure.py path/to/directory/containing/subdirectories/of/all/trials/

This code assumes that you have Fiji executable installed as $HOME/bin/fiji/ImageJ-linux64. Location of the executable can be modified in "multimeasure.py" file L30-31. Headless mode is faster but sometimes fail to execute for unknown reason. You may try it out by adding "--headless" flag to L30-31 after "$HOME/bin/fiji/ImageJ-linux64".

The result is saved as "Results.csv" in each of the "aligned" directory. These data can be further analyzed by the codes in "./data_analysis/GCaMP7f/statistics/". Just open each of the main~.R file with your favorite editor and execute line by line.
