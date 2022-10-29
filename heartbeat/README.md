This directory contains codes for acquiring and analyzing the heartbeat imaging data of the adult fly.

1. System requirements

1-1. Data acquisition

Supports Windows 7 operating system. This is currently our only supported platform.
No special requirements exist for CPU, RAM, Disk space/ speed. We have tested on a PC with Intel(R) Xeon(R) CPU E5-2650 0, 16GB RAM.
For control of a solenoid valve to implement air puff applications, Arduino/ Genuino Uno is required.
For acquisition of the heartbeat images, Leica SP8 confocal microscopy and its control software LASX (v1.1.0.12420) are required.
    
1-2. Data analysis

CPU (and accordingly RAM) usage can be relatively high due to parallel processing implementation, but the original code can be tweaked to limit the number of CPUs used. Requires Fiji (v2.3.0) for multi-ROI GCaMP quantification.



2. Installation guide

2-1. Instructions

Softwares
Please refer to the manufacturer's installation guide to install LASX (v1.1.0.12420) and Arduino IDE (v.1.8.19).



3. Instructions for use / Demo

3-1. Data acquisition

After transferring a fly under the microscope, execute the following command:
python record.py
Then the data acquisition should automatically proceed and complete within 17min. Be sure that "start" button on LASX is not overlaid by another window, as record.py involves virtual clicking of this button. The program generates image sequences of two trials per fly, each stored temporarily in the LASX software. You may rename each file later and save data to disk in a .lif format.To use our analysis code without modification, each file must be named like so: "DATEXXX_GENOTYPEXXX_DPEXXX_IDXXX_TRIALXXX.avi".

3-2. Data analysis

Each data stored in a lif file must first be exported to individual avi files. This can be done manually using Fiji, but we use a mini macro script to automate this process, like so:
ImageJ-linux64 -macro lif2avi.ijm path/to/dir/containing/lif/files/
This converts all the lif files stored in path into individual avi files in batch. Note that fiji may need to be open in another thread for this program to work.

The resultant avi files can then be converted into "heartbeat tracking" csv files, in batch, by: python avi2csv.py -d path/to/dir/containing/avi. It could take 10min to 20min to complete a single file, depending on your system, but our code process multiple files in parallel to minimize the overall processing time. 

Metadata (in json format) can be extracted from the name of csv files in batch by executing like so:
python get_metadata.py -d path/to/dir/containing/avi/files/

The csv and json files can then be used to analyze the heart rate etc. using our main.R code.
