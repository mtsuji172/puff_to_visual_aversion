#!/usr/bin/python3

import pandas as pd
import json
import os
import numpy as np
import sys

def get_metadata(csvfilename):
    print(csvfilename)

    # check if json file exists
    rjsonfilename = '/'.join(csvfilename.split('/')[:-2])+'/metadata/'+csvfilename.split('/')[-1].replace('.csv','.json')
    if not os.path.isfile(rjsonfilename):
        print(rjsonfilename+' not found! skipping...')
        return

    # read prediction result
    predictions = pd.read_csv(csvfilename, header=None)
    predictions.columns = ['label','filename']
    if '_mean' in str(predictions['filename'][0]):
        predictions['filename'] = predictions['filename'].apply(lambda x: x.replace('_mean',''))

    # read json file
    with open (rjsonfilename) as f:
        jsonfile = json.load(f)

    # get json info
    if 'jpg' in str(predictions['filename'][0]):
        frames = [int(x.split('_')[-1].replace('.jpg','')) for x in np.asarray(predictions['filename'])]
    else:
        frames = np.asarray(predictions['filename'])
    jsonfile_select = {key: np.repeat(value,len(frames)) for key, value in jsonfile.items() if key in ['genotype','ID','date','height','host','npuff','pufffreq','trial','postPuffInterval','LEDfreq']}
    jsonfile_select['frame'] = frames
    jsonfile_select_df = pd.DataFrame(jsonfile_select, index=range(len(frames)))

    # add json info to prediction result
    predictions = pd.concat([predictions, jsonfile_select_df], axis=1)

    # above procedure somehow translate "NA" into "" in the "label" column -> fix
    predictions = predictions.replace(np.nan, 'NA', regex=True)

    # output
    predictions.to_csv(csvfilename.replace('.csv','_labeled.csv'))
    return

if __name__=='__main__':
    get_metadata(sys.argv[1])
