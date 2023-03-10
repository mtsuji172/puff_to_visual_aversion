#!/usr/bin/python

from client import Client
import time
import math
import csv
import random
import threading
import os
import json

class Octant(Client):
    def __init__(self, *args, **kwargs):
        super(Octant, self).__init__(*args, **kwargs)
        self.clientscript = os.path.basename(__file__)
        self.gain = 0.0002
        self.t = 0
        self.t_end = 180
        self.angle_t0 = 0.0
        self.angleWidthOfIllumination = math.pi/4.0

    def isIlluminated(self, angle, angleWidthOfIllumination, arrangement):
        verdict = False
        illuminatedAngleStart = range(0, int(math.pi*2.0 / angleWidthOfIllumination), 2)

        for i in illuminatedAngleStart:
            if float(i)*angleWidthOfIllumination <= angle % (math.pi*2.0) < float(i+1)*angleWidthOfIllumination:
                verdict = True

        if arrangement == 'reverse':
            verdict = not verdict

        return verdict


    def main(self):
        # acclimation period
        print('acclimation...')
        time.sleep(self.args['acclimation'])

        # determine octant arrangement
        octantArrangement = random.choice(['right', 'reverse'])

        # activate optical sensor 
        self.control_opticalsensor(1)

        # warm up puff... needed to switch ON in the 1st loop
        print('warming up puff...')
        self.puff.switch('OFF')
        time.sleep(5)

        # initialize the mouse position
        self.initializeMouse()

        # start video recording 
        self.videotape()

        # main loop
        myrecord = []
        counter = 0
        angle = self.angle_t0
        t_start = time.time()
        while True:
            # timer
            t = time.time() - t_start
            if t > self.t_end:
                break

            # get the mouse position
            mouse_position = self.mouse.position()

            # if counter = 0, initialize stuff
            if counter == 0:
                t_prev = t
                x_prev = int(angle / self.anglePerPixel)
                mouse_position_x_prev = mouse_position[0]
                counter += 1
                continue

            # update the angle
            dx = mouse_position[0] - mouse_position_x_prev
            angle -= self.gain * 2 * math.pi * dx
            x = int(angle / self.anglePerPixel)

            # puff ON if in 'illumination' angle, OFF if otherwise
            if (self.isIlluminated(angle, self.angleWidthOfIllumination, octantArrangement) is True) & (self.puff.currentPuffState=='OFF'):
                print("in puff angle!")
                self.puff.switch('ON')
                self.puff.currentPuffState = 'ON'
            elif (self.isIlluminated(angle, self.angleWidthOfIllumination, octantArrangement) is False) & (self.puff.currentPuffState=='ON'):
                print("out of puff angle!")
                self.puff.switch('OFF')
                self.puff.currentPuffState = 'OFF'

            # record
            myrecord.append([self.puff.currentPuffState, angle, mouse_position[0], mouse_position[1]])

            # loop speed control
            self.controlLoopSpeed(self.loopInterval, t, t_prev)

            # if mouse pointer is beyond display boundaries, pull back to center
            mouse_position = self.inactivate_mouse_boundary(mouse_position)

            # record stuff for next loop
            mouse_position_x_prev = mouse_position[0]
            x_prev = x
            t_prev = t
            counter += 1

        # turn off puff
        self.puff.switch('OFF')

        # end the screen 
        self.s.send("1000")
        self.s.close()

        # inactivate optical sensor 
        self.control_opticalsensor(0)

        # export the record
        self.exportRecord(myrecord)
        self.exportMetaData()

        print('all done')


if __name__ == '__main__':
    Octant().main()
