import numpy as np
import pywt,cv2,sys, subprocess
import matplotlib.pyplot as plt

filename = "data/D-051-1_interpolated.txt"
imArray1 = np.loadtxt(filename)
imArray2 = imArray1.T
imArray3 = numpy.flip(imArray2, axis=None)
for i in [-10, -14.5, -15, -15.5, -16.5]:
	imArray4 = np.where(imArray3 < i, 0, 1)
	cv2.imwrite("data/D-051-1_thr_"+ str(i) + " .png", imArray4*255)