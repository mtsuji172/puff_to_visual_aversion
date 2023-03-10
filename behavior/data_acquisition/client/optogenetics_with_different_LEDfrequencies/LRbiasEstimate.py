#!/usr/bin/python

from client import Client
import time
import math
import csv
import random
import os

class SingleSquare4Closed(Client):
    def __init__(self, *args, **kwargs):
        super(SingleSquare4Closed, self).__init__(*args, **kwargs)
        self.clientscript = os.path.basename(__file__)
        self.gain = 0.0002
        self.width = int(self.args['width'])
        self.height = int(self.args['height'])
        self.y = 16 - self.height/2
        self.y2 = 16 + self.height/2
        self.t_end = 180
        self.angularPosition_t0 = random.choice([-math.pi/3, math.pi/3]) # in pi
        self.filename += '_CALIB'

    def main(self):
        # acclimation period
        print('acclimation...')
        time.sleep(self.args['acclimation'])

        # activate optical sensor 
        self.control_opticalsensor(1)

        # image processing
        self.videotape()
        
        # initialize mouse position
        self.initializeMouse()
        
        # main loop
        myrecord = []
        counter = 0
        angularPosition = self.angularPosition_t0
        while True:
            # if counter == 0, initialize stuff
            if counter == 0:
                t_start = time.time()
            t = time.time() - t_start
            if t > self.t_end:
                break
            
            # get the mouse position
            mouse_position = list(self.mouse.position())
            if counter == 0:
                mouse_position_x_prev = mouse_position[0]
                
            # update the angle, normalizing L-R bias. It is ABSOLUTELY important that the normalization takes place here, not in the mouse positon part, as sub-comma meanDx would end up being amplified to 1 (since mouse position only regards numbers as integers)
            dx = mouse_position[0] - mouse_position_x_prev
            dx_normalized = float(dx)
            angularPosition -= self.gain * 2.0 * math.pi * dx_normalized + float(str(self.args['AngularPosBias']).replace('"','').replace(' ',''))

            # convert angle to screen position 
            x, x2 = self.Angle2EdgesOfObjectInPixel(angularPosition)
            
            # if counter = 0, initialize stuff
            if counter == 0:
                x_prev = x
                t_prev = t
                counter += 1
                continue
            
            # record mouse position before controlling for boundary, to record the 'true' mouse position
            myrecord.append([angularPosition, mouse_position[0], mouse_position[1]])

            # update mouse position. Reflect this to mouse_position_prev so that the next dx won't 'jump', which then would cause the object to jump as well.
            mouse_position = self.inactivate_mouse_boundary(mouse_position)

            # loop speed control
            self.controlLoopSpeed(self.loopInterval, t, t_prev)
            
            # record
            t_prev = t
            x_prev = x
            mouse_position_x_prev = mouse_position[0]
            counter += 1

        # end the screen
        self.s.sendall("\n".join([str(500), str(0), str(0), str(0)]))

        # inactivate optical sensor 
        self.control_opticalsensor(0)

        # close the connection to raspberry pi
        self.s.close()

        # export the record
        self.exportRecord(myrecord)
        self.exportMetaData()

        print('all done')

if __name__ == '__main__':
    SingleSquare4Closed().main()
