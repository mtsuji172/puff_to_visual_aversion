#!/usr/bin/env python
from server import Server
import socket
import threading
import os

class SingleObject(Server):
    def __init__(self, *args, **kwargs):
        super(SingleObject, self).__init__(*args, **kwargs)
        self.serverscript = os.path.basename(__file__) 
        self.bgcolor = {"R":0, "G":0, "B":0}
        self.objectcolor = {"R":0, "G":5, "B":0}

    def run(self):
        # project canvas object
        offset_canvas = self.matrix.CreateFrameCanvas()
        self.clearScreen(offset_canvas)
        offset_canvas = self.matrix.SwapOnVSync(offset_canvas)
        
        # initiate server
        self.server(self.listenToClient4SingleObject)

# Main function
if __name__ == "__main__":
    singleobject = SingleObject()
    if (not singleobject.process()):
        singleobject.print_help()
