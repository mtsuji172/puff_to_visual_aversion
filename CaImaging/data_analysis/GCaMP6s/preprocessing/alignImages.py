import cv2
import numpy as np
from PIL import Image
import sys
from libtiff import TIFF
import os
import glob
from tqdm import tqdm
from pathlib import Path

class AlignImages(object):
    def __init__(self):
        pass

    def align(self, path2refimg, path2targetimg):
        imref = cv2.imread(path2refimg, cv2.CV_8UC1)
        imtarget = cv2.imread(path2targetimg, cv2.CV_8UC1)

        warp_mode = cv2.MOTION_AFFINE
        warp_matrix = np.eye(2, 3, dtype=np.float32)

        # Specify the number of iterations.
        number_of_iterations = 100

        # Specify the threshold of the increment in the correlation 
        # coefficient between two iterations
        termination_eps = 1e-7

        criteria = (cv2.TERM_CRITERIA_EPS | cv2.TERM_CRITERIA_COUNT, number_of_iterations, termination_eps)

        # Run the ECC algorithm. The results are stored in warp_matrix.
        (cc, warp_matrix) = cv2.findTransformECC(imref, imtarget, warp_matrix, warp_mode, criteria, inputMask=None, gaussFiltSize=1)

        # Get the target size from the desired image
        target_shape = imref.shape
        imref2 = np.array(TIFF.open(path2refimg).read_image())
        imtarget2 = np.array(TIFF.open(path2targetimg).read_image())
        aligned_image = cv2.warpAffine(
                                  imtarget2,
                                  warp_matrix,
                                  (target_shape[1], target_shape[0]),
                                  flags=cv2.INTER_LINEAR + cv2.WARP_INVERSE_MAP,
                                  borderMode=cv2.BORDER_CONSTANT,
                                  borderValue=0)
        path2targetimg_split = path2targetimg.split('/')
        path2alignedimg = '/'.join(path2targetimg_split[:-2])+'/aligned/'+path2targetimg_split[-1].replace('.tif','_aligned.tif')
        path2alignedrefimg = '/'.join(path2targetimg_split[:-2])+'/aligned/'+path2refimg.split('/')[-1].replace('.tif','_aligned.tif')
        cv2.imwrite(path2alignedrefimg, imref2)
        cv2.imwrite(path2alignedimg, aligned_image)

    def main(self, path2targetdir, refimg): #path2targetdir = "../data_new/"
        # scan target dir
        mydirs = next(os.walk(path2targetdir))[1] # -> e.g. ['20200827_ID1_1','XXX'...]
        counter=0
        for mydir in mydirs:
            print('processing '+ mydir +' ...'+str(counter)+'/'+str(len(mydirs)))
            # sort files
            indir = path2targetdir+mydir+'/zprojection/'
            Path(indir+'original').mkdir(parents=True, exist_ok=True)
            Path(indir+'aligned').mkdir(parents=True, exist_ok=True)
            os.system("mv "+indir+"*.tif "+indir+"original/")

            # scan original files
            files_all = glob.glob (indir+"original/*.tif")
            files = [myfile for myfile in files_all if not any(x in myfile for x in (refimg, '_aligned'))]

            # align original files and store them into aligned folder
            bar = tqdm(range(len(files)))
            for myfile in files:
                self.align(indir+'original/'+refimg, myfile)
                bar.update(1)

            counter += 1

if __name__=='__main__':
    AlignImages().main(sys.argv[1], 'ChanA_0001.tif')
