#!/usr/bin/python3

from airpuff import AirPuff
import time
import pymouse

# params
recordingDuration = 60
interval=90
acclimation = 600
numtrials = 2
tx = 1200
ty = 1560

# create airpuff object
arduino = AirPuff()
mouse = pymouse.PyMouse()

def record():
    # click on "record"
    print('recording start...')
    mouse.click(tx, ty)

    # wait for recordingDurations
    time.sleep(recordingDuration)

    # puff start
    print('puff start...')
    for i in range(10):
        arduino.switch('ON')
        time.sleep(0.5)
        arduino.switch('OFF')
        time.sleep(0.5)

    # wait for recordingDurations
    time.sleep(recordingDuration)

    # click on "record"
    print('recording done')
    mouse.click(tx, ty)


### main
print("acclimation of 10min...")
time.sleep(acclimation)
print('recording of '+str(numtrials)+' trials start...')
print("acclimation...")
for i in range(numtrials):
    print('[*] trial '+str(i))
    record()
    time.sleep(interval)
print('all recording done')
