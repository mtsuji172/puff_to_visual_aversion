#!/usr/bin/python

import pymouse
import time
import datetime
import serial
import math
import sys
import os
import argparse
import subprocess
import socket
import airpuff
import csv
import json

class Client(object):
    def __init__(self):
        self.port = 12345
        self.y_max = 32
        self.x_max = 16
        self.n_device = 6 
        self.x_max_tot = self.x_max * self.n_device  
        self.anglePerPixel = (2*math.pi)/(self.x_max*8)
        self.angle_tot = 2*math.pi * self.n_device/8
        self.loopInterval = 0.01 # in sec
        self.mouse = pymouse.PyMouse()
        self.mouseBoundary = [1800, 1000]
        self.mouseCenter = [1000, 500]
        self.puff = airpuff.AirPuff() 
        self.readArguments()
        self.initializeSocket()
        self.datetime = datetime.datetime.now()  
        # determine outpuf file name
        self.filename = self.datetime.strftime("%Y.%m.%d")+'_GENOTYPE'+self.args['genotype']+'_ID'+str(self.args['ID'])+'_PUFF'+self.args['puff']+'_TRIAL'+str(self.args['trial'])
    
    def readArguments(self):
        # build argument parser
        ap = argparse.ArgumentParser()
        ap.add_argument("--host", type=str, help="ip of host raspi")
        ap.add_argument("-p", "--puff", type=str, default='False', help="True/False")
        #ap.add_argument("--LED", type=str, default='False', help="True/False")
        ap.add_argument("--genotype", type=str, default='CS', help="genotype of sample")
        ap.add_argument("--ID", type=int, default=0, help="identifier No of the sample")
        ap.add_argument("--trial", type=int, default=0, help="trial No")
        ap.add_argument("--postPuffInterval", type=int, default=0, help="post puff interval")
        ap.add_argument("-a", "--acclimation", type=int, default=300, help="acclimation period")
        ap.add_argument("--npuff", type=int, default=10, help="number of puffs applied")
        #ap.add_argument("--diameter", type=int, default=8, help="width/ height of the visual object")
        ap.add_argument("--width", type=int, default=8, help="width/ height of the visual object")
        ap.add_argument("--height", type=int, default=8, help="width/ height of the visual object")
        ap.add_argument("--AngularPosBias", type=float, default=0.0, help="mean L-R bias as estimated for each fly")
        #ap.add_argument("--LEDONcue", type=str, default='B', help="LED-ON visual cue")
        args = vars(ap.parse_args())
        if args['host'] is None:
            sys.exit('You need to specify host ip! Quitting...')
        self.args = args

    def exportRecord(self, myrecord):
        with open('../data/'+self.filename+'.csv', 'w') as f:
            a = csv.writer(f)
            a.writerows(myrecord)

    def exportMetaData(self):
        metadata = self.args
        metadata['clientscript'] = self.clientscript
        metadata['date'] = self.datetime.strftime("%Y.%m.%d")
        metadata['time'] = self.datetime.strftime("%H")
        del metadata['puff']
        metadata.update(self.serverMeta)
        metadata_sorted = {k: metadata[k] for k in sorted(metadata)}
        with open('../data/'+self.filename+'.json', 'w') as fp:
            json.dump(metadata_sorted, fp)

    def receiveMetadata(self, connection):
        data = connection.recv(1024)
        data = json.loads(data.decode('utf-8'))
        self.serverMeta = data
        print('[*] server metadata received')

    def initializeSocket(self):
        # establish connection to raspberry pi controlling adafruit LED matrices
        print('trying to connected to raspberrypi on port '+str(self.port)+', ip: '+self.args['host']+'...')
        s = socket.socket()
        s.connect((self.args['host'], self.port))
        print('[*] connected to raspberrypi')
        self.receiveMetadata(s)
        self.s = s

    def controlLoopSpeed(self, loopInterval, t, t_prev):
            if t - t_prev > loopInterval:
                print('loop slow')
                pass
            else:
                time.sleep(loopInterval - (t - t_prev))

    def virtual2realAngle(self, angularPosition):
        # in virtual space, angularPosition spans from -pi to pi.
        # in reality, angularPosition spans from 0 to 2*pi*n_device/8
        # e.g. 0 degree in virtual space corresponds to 2*pi*n_device/8 / 2
        # correct for this gap
        return angularPosition + math.pi * self.n_device/8

    def Angle2EdgesOfObjectInPixel(self, angularPosition):
        # no need for % self.x_max_tot, as this normalization is to be done by raspi server
        CenterOfObjectInPixel = int(self.virtual2realAngle(angularPosition) / self.anglePerPixel)
        x = (CenterOfObjectInPixel - self.width/2)
        x2 = (CenterOfObjectInPixel + self.width/2)
        return x, x2

    def control_opticalsensor(self, OnOff):
        os.system('bash control_opticalsensor.sh '+str(OnOff))

    def initializeMouse(self):
        self.mouse.move(self.mouseCenter[0],self.mouseCenter[1])
    
    def inactivate_mouse_boundary(self, mouse_position):
        mouse_position = list(mouse_position)
        if mouse_position[1] <= 0 or mouse_position[1] >= self.mouseBoundary[1]:
            self.mouse.move(mouse_position[0],self.mouseCenter[1])
            mouse_position[1] = self.mouseCenter[1]
        
        if mouse_position[0] <= 0 or mouse_position[0] >= self.mouseBoundary[0]:
            self.mouse.move(self.mouseCenter[0], mouse_position[1])
            mouse_position[0] = self.mouseCenter[0]
        
        return mouse_position
    
    def videotape(self):
        subprocess.Popen(['/usr/bin/python','videotape.py','-f',self.filename,'-r',str(int(self.t_end))])

    def apply_puff(self, npuff):
        print('now puffing...')
        if self.args['puff']=='True':
            for i in range(npuff):
                self.puff.switch('ON')
                time.sleep(0.5)
                self.puff.switch('OFF')
                time.sleep(0.5)
        else:
            for i in range(npuff):
                time.sleep(0.5)
                time.sleep(0.5)
