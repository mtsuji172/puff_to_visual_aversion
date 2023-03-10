#!/usr/bin/python

import os
import argparse
import time
from random import shuffle
import itertools as it
import numpy as np
import csv
import pandas as pd
from datetime import datetime
from datetime import timedelta

# construct parser
parser = argparse.ArgumentParser()
parser.add_argument("--host", type=str, help="ip of host raspi")
parser.add_argument("--genotype", type=str, default='CS', help="genotype of sample")
parser.add_argument("-p", "--puff", type=str, default='False', help="True/False")
parser.add_argument("--ID", type=int, default=0, help="identifier No of the sample")
parser.add_argument("--postPuffInterval", type=int, default=0, help="post puff interval")
parser.add_argument("-a", "--acclimation", type=int, default=0, help="acclimation period")
parser.add_argument("--preRepeatAcclimation", type=int, default=1020, help="acclimation period before start of repeat trials")
args = vars(parser.parse_args())

# determine the systematic + fly's L-R bias
print('acclimation')
time.sleep(args['preRepeatAcclimation'])
print('determining L-R bias')
os.system('python LRbiasEstimate.py --host '+args['host']+ ' --acclimation 0 --genotype ' + args['genotype'] + ' --ID ' + str(args['ID']))
filename = '../data/'+datetime.now().strftime("%Y.%m.%d")+'_GENOTYPE'+args['genotype']+'_ID'+str(args['ID'])+'_PUFFFalse_TRIAL0_CALIB.csv'
mydata = pd.read_csv(filename)
angularpos = mydata.iloc[:,0]
dangularpos = np.diff(angularpos)
AngularPosBias = np.mean(dangularpos[(dangularpos>=np.quantile(dangularpos, 0.05)) & (dangularpos<=np.quantile(dangularpos, 0.95))])
print AngularPosBias

# recording (trialtype='normal')
puff = True
command = 'python  closed_virtualoctant_stimulation.py --host ' + str(args['host']) + ' --acclimation '+ str(args['acclimation']) + ' -p ' + str(puff) + ' --npuff 1 --postPuffInterval ' + str(args['postPuffInterval']) + ' --genotype ' + args['genotype'] + ' --width 2 --height 2 --ID ' + str(args['ID']) + ' --trial 0 --AngularPosBias " ' + str(AngularPosBias)+'"' # negative value would be mistakenly recognized as flags if not given in a " -XXX" format

print('executing: '+ command)
os.system(command)
