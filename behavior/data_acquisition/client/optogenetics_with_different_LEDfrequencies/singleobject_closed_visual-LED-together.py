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
        self.t_end = 60
        self.angularPosition_t0 = random.choice([-math.pi/3, math.pi/3]) # in pi

    def main(self):
        # update LEDfreq of arduino
        if self.args['LEDfreq'] == 0.0:
            LEDboutlen = 0
        else:
            LEDboutlen = int(1000.0 / self.args['LEDfreq'] / 2.0)
        os.system("sed -i 's/delay(mydelay)/delay("+str(LEDboutlen)+")/g' ../arduino_puffLED/arduino_puffLED.ino")
        print('/home/masato/bin/arduino/arduino --upload --port '+self.arduino.LEDboard+' ../arduino_puffLED/arduino_puffLED.ino')
        os.system('/home/masato/bin/arduino/arduino --upload --port '+self.arduino.LEDboard+' ../arduino_puffLED/arduino_puffLED.ino')

        # acclimation period
        print('acclimation...')
        time.sleep(self.args['acclimation'])

        # puff
        if self.args['pufffreq'] > 0:
            self.arduino.switchPuff("ON")
        time.sleep(self.puffduration - 0.05)
        if self.args['pufffreq'] > 0:
            self.arduino.switchPuff("OFF")
        
        # post-puff interval
        time.sleep(self.args['postPuffInterval'])

        # activate optical sensor 
        self.control_opticalsensor(1)

        # image processing
        self.videotape()
        
        # initialize mouse position
        self.initializeMouse()
        
        # LED ON
        if self.args['LEDfreq'] > 0:
            self.arduino.switchLED("ON")

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
                self.s.sendall("\n".join([str(x), str(self.y), str(x2), str(self.y2)]))
                counter += 1
                continue
            
            # update the screen (only if normalized x changes)
            if x != x_prev:
                # write screen
                self.s.sendall("\n".join([str(x), str(self.y), str(x2), str(self.y2)]))
            
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
        
        # LED OFF
        self.arduino.switchLED("OFF")

        # end the screen (in rare occations, this communication fails -> visual object remains displayed until the next recording begins -> repeat for 3times to be on the safe side)
        for i in range(3):
            self.s.sendall("\n".join([str(500), str(0), str(0), str(0)]))
            time.sleep(1)

        # inactivate optical sensor 
        self.control_opticalsensor(0)

        # close the connection to raspberry pi
        self.s.close()

        # export the record
        self.exportRecord(myrecord)
        self.exportMetaData()
        
        # revert the content of LED arduino
        os.system("sed -i 's/delay("+str(LEDboutlen)+")/delay(mydelay)/g' ../arduino_puffLED/arduino_puffLED.ino")
        print('all done')

if __name__ == '__main__':
    SingleSquare4Closed().main()
