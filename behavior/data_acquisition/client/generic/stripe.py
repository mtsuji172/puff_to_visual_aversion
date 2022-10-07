#!/usr/bin/python

from client import Client
import time
import math
import csv
import random
import os

class Stripe(Client):
    def __init__(self, *args, **kwargs):
        super(Stripe, self).__init__(*args, **kwargs)
        self.clientscript = os.path.basename(__file__)
        self.sinfreq = 22.4/360.0
        self.t = 0
        self.t_startRotation1  = 5
        self.t_endRotation1 = 15
        self.t_startRotation2  = 20
        self.t_endRotation2 = 30
        self.t_end = 35
        self.whichfirst = random.choice(['R','L'])

    def main(self):
        # acclimation period
        print('acclimation...')
        time.sleep(self.args['acclimation'])

        # puff
        self.apply_puff()

        # activate optical sensor 
        self.control_opticalsensor(1)

        # initialize the mouse position
        self.initializeMouse()

        # image processing
        self.videotape()

        # main loop
        myrecord = []
        counter = 0
        angle = 0
        movingDirection = 0
        t_start = time.time()
        while True:
            print(counter)
            # if counter = 0, initialize stuff
            if counter == 0:
                x_prev = int(angle / self.anglePerPixel)
                angle_prev = angle
                t_prev = t_start
                counter += 1
                continue

            # timer
            t = time.time() - t_start
            if t > self.t_end:
                break

            # get the mouse position
            mouse_position = self.mouse.position()

            # update the angle and corresponding LED positions
            if t >= self.t_startRotation1 and t < self.t_endRotation1:
                movingDirection = 1 if self.whichfirst == 'R' else -1
            elif t >= self.t_startRotation2 and t < self.t_endRotation2:
                movingDirection = - 1 if self.whichfirst == 'R' else 1
            else:
                movingDirection = 0

            angle += movingDirection * self.sinfreq * 2 * math.pi * (t - t_prev)
            x = int(angle / self.anglePerPixel)

            # update the LED (only if normalized x changes)
            if x != x_prev:
                # write screen
                self.s.send(str(x))

            # update mouse position
            self.inactivate_mouse_boundary(mouse_position)

            # record
            myrecord.append([angle, mouse_position[0], mouse_position[1]])

            # loop speed control
            print(t-t_prev)
            #self.controlLoopSpeed(self.loopInterval, t, t_prev)

            # record current normalized y and update counter
            t_prev = t
            angle_prev = angle
            x_prev = x
            counter += 1

        # end the screen 
        self.s.send("1000")
        self.s.close()

        # inaactivate optical sensor 
        self.control_opticalsensor(0)

        # export the record
        self.exportRecord(myrecord)
        self.exportMetaData()

        print('all done')


if __name__ == '__main__':
    Stripe().main()
