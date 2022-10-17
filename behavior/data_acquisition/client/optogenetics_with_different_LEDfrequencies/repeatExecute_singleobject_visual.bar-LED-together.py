#!/usr/bin/python
print("\n\ncheck ard.py so /dev/ttyACM* match 2 boards!\n\n")

import os
import argparse
import time
from random import shuffle
import itertools as it
import numpy as np
import csv
import pandas as pd
from datetime import datetime

# construct parser
parser = argparse.ArgumentParser()
parser.add_argument("--host", type=str, help="ip of host raspi")
parser.add_argument("--genotype", type=str, default='CS', help="genotype of sample")
parser.add_argument("-p", "--puff", type=str, default='False', help="True/False")
parser.add_argument("--ID", type=int, default=0, help="identifier No of the sample")
parser.add_argument("--postPuffInterval", type=int, default=0, help="post puff interval")
parser.add_argument("-a", "--acclimation", type=int, default=180, help="acclimation period")
parser.add_argument("--preRepeatAcclimation", type=int, default=1020, help="acclimation period before start of repeat trials")
args = vars(parser.parse_args())

# determine the schedule for test
mydict = {'1.pufffreq':[0.0], '2.LEDfreq':[0.0,6.0,12.0,60.0]}
allnames= sorted(mydict)
print(allnames)
combinations=it.product(*(mydict[name] for name in allnames))
schedule = list(combinations) * 3
shuffle(schedule)
print(schedule)

# set color
print('setting color..')
command = 'python setcolorON.py --host ' + str(args['host']) + ' --acclimation 0 -p False --pufffreq 0.0 --postPuffInterval 0 --genotype setcolor --width 1 --height 1 --ID 0 --trial 0 --AngularPosBias 0'
os.system(command)

# determine the systematic + fly's L-R bias
print('\n\nacclimation')
time.sleep(args['preRepeatAcclimation'])
print('\n\ndetermining L-R bias')
os.system('python LRbiasEstimate.py --host '+args['host']+ ' --acclimation 0 --genotype ' + args['genotype'] + ' --ID ' + str(args['ID']) + ' --LEDfreq 0')
filename = '../data/'+datetime.now().strftime("%Y.%m.%d")+'_GENOTYPE'+args['genotype']+'_ID'+str(args['ID'])+'_PUFFFalse_TRIAL0_CALIB.csv'
mydata = pd.read_csv(filename)
angularpos = mydata.iloc[:,0]
dangularpos = np.diff(angularpos)
AngularPosBias = np.mean(dangularpos[(dangularpos>=np.quantile(dangularpos, 0.05)) & (dangularpos<=np.quantile(dangularpos, 0.95))])
print AngularPosBias

# recording
for i in range(len(schedule)):
    puff = 'True' if schedule[i][0]>0 else 'False'
    command = 'python singleobject_closed_visual-LED-together.py --host ' + str(args['host']) + ' --acclimation '+ str(args['acclimation']) + ' -p '+puff+' --pufffreq '+str(schedule[i][0])+' --LEDfreq ' + str(schedule[i][1]) + ' --postPuffInterval ' + str(args['postPuffInterval']) + ' --genotype ' + args['genotype'] + ' --width 8 --height 32 --ID ' + str(args['ID']) + ' --trial ' + str(i) + ' --AngularPosBias " ' + str(AngularPosBias)+'"' # negative value would be mistakenly recognized as flags if not given in a " -XXX" format

    print('\n\nexecuting: '+ command)
    os.system(command)

# reverting the color
command = 'python setcolorOFF.py --host ' + str(args['host']) + ' --acclimation 0 -p False --pufffreq 0.0 --postPuffInterval 0 --genotype setcolor --width 1 --height 1 --ID 0 --trial 0 --AngularPosBias 0'
os.system(command)
print('\n\n all done!')
