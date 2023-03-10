#!/usr/bin/env python
from server import Server
import socket
import threading
import os
class Stripe(Server):
    def __init__(self, *args, **kwargs):
        super(Stripe, self).__init__(*args, **kwargs)
        self.serverscript = os.path.basename(__file__)
        self.bgcolor = {"R":0, "G":0, "B":0}
        self.objectcolor = {"R":0, "G":0, "B":0}
        self.stripeWidth = 8

    def fillStripe(self, my_canvas, phase):
        self.clearScreen(my_canvas)
        xVirtualStartPositions = [i for i in range(self.xdimTot) if i % (self.stripeWidth*2) == phase]
	for x in xVirtualStartPositions:
            if x+self.stripeWidth < self.xdimTot:
		self.fillRectangle(my_canvas, x, x+self.stripeWidth, 0, self.ydim)
            else:
		self.fillRectangle(my_canvas, x, self.xdimTot, 0, self.ydim)
		self.fillRectangle(my_canvas, 0, (x + self.stripeWidth) % self.xdimTot, 0, self.ydim)
        return my_canvas

    def prepareAllPhasesOfStripe(self):
        offset_canvas_allPhases = [self.matrix.CreateFrameCanvas() for i in range(self.stripeWidth*2)]
        offset_canvas_allPhases = [self.fillStripe(offset_canvas_allPhases[i], i) for i in range(len(offset_canvas_allPhases))]
        return offset_canvas_allPhases
	
    def listenToClient4Stripe(self, c, addr):
        # prepare canvas object
        self.offset_canvas_allPhases = self.prepareAllPhasesOfStripe()

        # initiate loop
        while True:
            # receive data
            data = int(c.recv(1024))
            if data==500:
                print('closing connection from', addr)
                self.offset_canvas = self.matrix.SwapOnVSync(self.offset_canvas_allPhases[0])
                break
            
            elif data>=1000:
                print('received '+str(data))
                if str(data)[2]=='0':
                    self.bgcolor["R"] = int(str(data)[3:])
                elif str(data)[2]=='1':
                    self.bgcolor["G"] = int(str(data)[3:])
                elif str(data)[2]=='2':
                    self.bgcolor["B"] = int(str(data)[3:])
                elif str(data)[2]=='3':
                    self.objectcolor["R"] = int(str(data)[3:])
                elif str(data)[2]=='4':
                    self.objectcolor["G"] = int(str(data)[3:])
                elif str(data)[2]=='5':
                    self.objectcolor["B"] = int(str(data)[3:])
                elif str(data)[2]=='9':
                    print('updating screen')
                    print('bgcolor: ')
                    print(self.bgcolor)
                    print('objectcolor: ')
                    print(self.objectcolor)
                    # prject screen to canvas
                    self.offset_canvas_allPhases = self.prepareAllPhasesOfStripe()
                    self.offset_canvas = self.offset_canvas_allPhases[0] 
                    self.offset_canvas = self.matrix.SwapOnVSync(self.offset_canvas)
                    print('screen updated')

            else:
                self.offset_canvas = self.matrix.SwapOnVSync(self.offset_canvas_allPhases[data % (self.stripeWidth*2)])
    
    def run(self):
        # prject screen to canvas
        self.offset_canvas_allPhases = self.prepareAllPhasesOfStripe()
        self.offset_canvas = self.offset_canvas_allPhases[0] 
        self.offset_canvas = self.matrix.SwapOnVSync(self.offset_canvas)
        
        # wait for client access
        self.server(self.listenToClient4Stripe)

# Main function
if __name__ == "__main__":
    stripe = Stripe()
    if (not stripe.process()):
        stripe.print_help()
