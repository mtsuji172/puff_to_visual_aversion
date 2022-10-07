#!/usr/bin/python3
# python processImages.py ../Tk-GCaMP6s/

from alignImages import AlignImages
import os
import sys

# G-R and maxproject along z, for each t
os.system('bash processImages.sh '+sys.argv[1])
print('[*] G-R and maxprojections done!')

# align maxprojected images
print('aligning images...')
AlignImages().main(sys.argv[1], 'ChanA_0001.tif')
