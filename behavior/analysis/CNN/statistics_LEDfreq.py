#!/usr/bin/python3

import pandas as pd
import numpy as np
from progress.bar import IncrementalBar
import multiprocessing
import argparse
import os
from pathlib import Path
import csv
from tqdm import tqdm
import glob

# construct the argument parser and parse the arguments
ap = argparse.ArgumentParser()
ap.add_argument("-p", "--predictions", help="full path to prediction_all_labeled.csv")
ap.add_argument("-o", "--outdir", required=True, help="full path to dir to store output")
args = vars(ap.parse_args())

# params
maxframe = 49 # corresponds to 5s
labels = ['stuck','freezeInAir','flight','walk','stop','glooming','PER']
nlabel = len(labels)

# functons
def completePath(path):
    if path[-1]!='/':
        path += '/'
    return path

def check_coverage(mydata):
    coverage = mydata.label.count() / len(mydata.label)
    if coverage > 0.7:
        return 'pass'
    else:
        return 'fail'

def calc_framemean(uniq_trialtype_LEDfreq_pufffreq):
    subdata = mydata[mydata.uniq_trialtype_LEDfreq_pufffreq==uniq_trialtype_LEDfreq_pufffreq]
    result_all = []
    for frame in subdata.frame:
        subdata_frame = subdata[subdata.frame==frame]
        myratio =  [subdata_frame.label.tolist().count(x) / subdata_frame.shape[0] for x in labels]
        result = subdata_frame.iloc[0][['filename','frame','genotype','pufffreq','trial','uniq','trialtype','LEDfreq','postPuffInterval','date','host','uniq_trialtype_LEDfreq_pufffreq']]
        result['filename'] = '_'.join(result['filename'].split('_')[:-1])
        result = pd.concat([pd.DataFrame(result).transpose()]*nlabel)
        result['label'] = labels
        result['ratio'] = myratio
        result_all.append(result)
    result_all = pd.concat(result_all, axis=0)
    return result_all

def calc_trialmean(mydata):
    myratio =  [mydata.label.tolist().count(x) / mydata.shape[0] for x in labels]
    result = mydata.iloc[0][['filename','genotype','pufffreq','trial','uniq','trialtype','LEDfreq','postPuffInterval','date','host','uniq_trial_trialtype_LEDfreq']]
    result = pd.concat([pd.DataFrame(result).transpose()]*nlabel)
    result['label'] = labels
    result['ratio'] = myratio
    return result

#################################
# main
#################################
# prep outdir
path2prediction = completePath(args['predictions'])
outdir = completePath(args['outdir'])
Path(outdir).mkdir(parents=True, exist_ok=True)
Path(outdir+'/trialmean/').mkdir(parents=True, exist_ok=True)

# list prediction files
myfiles = glob.glob(path2prediction+"/*_labeled.csv")
for myfile in tqdm(myfiles):
    # read a file
    mydata = pd.read_csv(myfile)
    mydata.label = mydata.label.str.extract(r"\['(.*)'\]")
    mydata = mydata.iloc[:,1:]

    # add some info
    trialtype = np.repeat('normal', len(mydata))
    trialtype[['CALIB' in x for x in np.asarray(mydata.filename)]] = 'CALIB'
    mydata['trialtype'] = trialtype
    del trialtype
    mydata['uniq'] = mydata.genotype+'_'+mydata.ID.astype(str)+'_'+mydata.date
    mydata['uniq_trialtype_LEDfreq_pufffreq'] = mydata.uniq+'_'+mydata.trialtype+'_'+mydata.LEDfreq.astype(str)+'_'+mydata.pufffreq.astype(str)
    mydata['uniq_trial_trialtype_LEDfreq'] = mydata.uniq+'_'+mydata.trial.astype(str)+'_'+mydata.trialtype+'_'+mydata.LEDfreq.astype(str)

    # omit trials with insufficient coverage
    if check_coverage(mydata) =='fail':
        continue

    # omit NA rows
    mydata = mydata.dropna()

    # omit trials with insufficient coverage
    if len(mydata)==0:
        continue

    # count #labels
    mydata_select = mydata.copy()
    mydata_select = mydata_select[mydata_select.frame < maxframe]
    trialmean = calc_trialmean(mydata_select)

    # select & output
    trialmean.to_csv(outdir+'/trialmean/'+myfile.split('/')[-1].replace('_labeled',''), index=False)
