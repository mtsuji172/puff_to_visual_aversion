import cv2
import numpy as np
from PIL import Image, ImageOps
import sys
from libtiff import TIFF
import os
import glob
from pathlib import Path
import multiprocessing as mp

class AlignImages(object):
    def __init__(self):
        self.refimg = 'ChanA_0001_0001_0001_0001.tif'

    def pil2cv(self, image):
        new_image = np.array(image, dtype=np.uint8)
        if new_image.ndim == 2:  # モノクロ
            pass
        elif new_image.shape[2] == 3:  # カラー
            new_image = cv2.cvtColor(new_image, cv2.COLOR_RGB2BGR)
        elif new_image.shape[2] == 4:  # 透過
            new_image = cv2.cvtColor(new_image, cv2.COLOR_RGBA2BGRA)
        return new_image

    def align(self, path2refimg, path2targetimg):
        # read images
        imref = Image.open(path2refimg).convert("L")
        imtarget = Image.open(path2targetimg).convert("L")

        # autocontrast (this improves findECC)
        imref = self.pil2cv(ImageOps.autocontrast(imref, cutoff=3))
        imtarget = self.pil2cv(ImageOps.autocontrast(imtarget, cutoff=3))

        # findECC
        warp_mode = cv2.MOTION_TRANSLATION
        warp_matrix = np.eye(2, 3, dtype=np.float32)
        number_of_iterations = 100
        termination_eps = 1e-7
        criteria = (cv2.TERM_CRITERIA_EPS | cv2.TERM_CRITERIA_COUNT, number_of_iterations, termination_eps)

        try:
            # Run the ECC algorithm. The results are stored in warp_matrix.
            try:
                (cc, warp_matrix) = cv2.findTransformECC(imref, imtarget, warp_matrix, warp_mode, criteria, inputMask=None, gaussFiltSize=1)
            except TypeError:
                (cc, warp_matrix) = cv2.findTransformECC(imref, imtarget, warp_matrix, warp_mode, criteria)

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
            cv2.imwrite(path2alignedimg, aligned_image)
        except Exception as e:
            return e

    def main(self, path2targetdir, refimg): #path2targetdir = "../data_new/ID0/GminusR/"
        # init pool
        pool = mp.Pool(mp.cpu_count()-2)

        # sort files
        Path(path2targetdir+'original').mkdir(parents=True, exist_ok=True)
        Path(path2targetdir+'aligned').mkdir(parents=True, exist_ok=True)
        os.system("cp "+path2targetdir+self.refimg+" "+path2targetdir+"aligned/")
        os.system("mv "+path2targetdir+"*.tif "+path2targetdir+"original/")

        # scan original files
        files_all = glob.glob(path2targetdir+"original/*.tif")
        files = [myfile for myfile in files_all if not any(x in myfile for x in (refimg, '_aligned'))]
        # align original files and store them into aligned folder
        pool.starmap(self.align, [(path2targetdir+'original/'+refimg, myfile) for myfile in files])

        pool.close()

if __name__=='__main__':
    AlignImages().main(sys.argv[1]+'/', self.refimg)
