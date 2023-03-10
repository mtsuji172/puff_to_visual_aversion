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
        self.t_end = 10
        self.angularPosition_t0 = random.choice([-math.pi/3, math.pi/3]) # in pi

    def main(self):
        # set the bg color
        self.s.sendall("\n".join([str(501), str(0), str(5), str(0)]))
        time.sleep(5)

        # set the object color
        self.s.sendall("\n".join([str(502), str(0), str(0), str(0)]))

        # close the connection to raspberry pi
        self.s.close()

        print('all done')

if __name__ == '__main__':
    SingleSquare4Closed().main()
