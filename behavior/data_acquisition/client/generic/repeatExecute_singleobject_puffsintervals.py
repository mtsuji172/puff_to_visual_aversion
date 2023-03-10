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
parser.add_argument("-a", "--acclimation", type=int, default=90, help="acclimation period")
parser.add_argument("--preRepeatAcclimation", type=int, default=1020, help="acclimation period before start of repeat trials")
args = vars(parser.parse_args())

# determine the schedule for test
mydict = {'1.width':[8], '2.height':[8], '3.npuff':[0,10], '4.interval':[0,1,3,6,10]}
allnames= sorted(mydict)
print(allnames)
combinations=it.product(*(mydict[name] for name in allnames))
schedule = list(combinations) * 1
shuffle(schedule)
print(schedule)

## determine the schedule for activity tests
#mydict = {'1.width':[2], '2.height':[2], '3.npuff':[0,1,5,10], '4.interval':[0,3,6,10]}
#allnames= sorted(mydict)
#print(allnames)
#combinations=it.product(*(mydict[name] for name in allnames))
#schedule_activity = list(combinations) * 2
#shuffle(schedule_activity)
#print(schedule_activity)
#
## indicate ETA
#totalduration = args['preRepeatAcclimation']+180 + (args['acclimation']+args['postPuffInterval']+5)*(len(schedule)+len(schedule_activity))
#ETA = datetime.now() + timedelta(seconds=totalduration)
#print('NOW: '+datetime.now().strftime("%H:%m"))
#print('total duration: '+str(totalduration/60)+'min')
#print("ETA: "+ETA.strftime("%H:%m")+"\n")

# set color
print('setting color..')
command = 'python setcolorON.py --host ' + str(args['host']) + ' --acclimation 0 -p False --npuff 0 --postPuffInterval 0 --genotype setcolor --width 1 --height 1 --ID 0 --trial 0 --AngularPosBias 0'
os.system(command)

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
for i in range(len(schedule)):
    puff = True if schedule[i][2]!=0 else False
    command = 'python singleobject_closed.py --host ' + str(args['host']) + ' --acclimation '+ str(args['acclimation']) + ' -p ' + str(puff) + ' --npuff ' + str(schedule[i][2]) + ' --postPuffInterval ' + str(schedule[i][3]) + ' --genotype ' + args['genotype'] + ' --width ' + str(schedule[i][0]) + ' --height ' + str(schedule[i][1]) + ' --ID ' + str(args['ID']) + ' --trial ' + str(i) + ' --AngularPosBias " ' + str(AngularPosBias)+'"' # negative value would be mistakenly recognized as flags if not given in a " -XXX" format

    print('executing: '+ command)
    os.system(command)

## recording (trialtype='ACTIVITY')
#for i in range(len(schedule_activity)):
#    puff = True if schedule_activity[i][2]!=0 else False
#    command = 'python test_activity.py --host ' + str(args['host']) + ' --acclimation '+ str(args['acclimation']) + ' -p ' + str(puff) + ' --npuff ' + str(schedule_activity[i][2]) + ' --postPuffInterval ' + str(schedule[i][3]) + ' --genotype ' + args['genotype'] + ' --width ' + str(schedule_activity[i][0]) + ' --height ' + str(schedule_activity[i][1]) + ' --ID ' + str(args['ID']) + ' --trial ' + str(i) + ' --AngularPosBias " ' + str(AngularPosBias)+'"' # negative value would be mistakenly recognized as flags if not given in a " -XXX" format
#
#    print('executing: '+ command)
#    os.system(command)

# reverting the color
command = 'python setcolorOFF.py --host ' + str(args['host']) + ' --acclimation 0 -p False --npuff 0 --postPuffInterval 0 --genotype setcolor --width 1 --height 1 --ID 0 --trial 0 --AngularPosBias 0'
os.system(command)
print(' all done!')
