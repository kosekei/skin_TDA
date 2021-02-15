#execute: python3 script_path image_path min_wavelet_level max_wavelet_level erosion_times R_script_path output0 output1

import numpy as np
import pandas as pd
import pywt,cv2,sys,subprocess,homcloud,os
import matplotlib.pyplot as plt

args = sys.argv
image_path = args[1] #jpg file
min_wavelet_level = args[2] #int
max_wavelet_level = args[3] #int
erosion_times = args[4] #int
R_script_path = args[5] #path of RTDA.R
output0 = args[6] #txt file
output1 = args[7] #txt file

def preprocess(image_path, coordinate_data_path, min_wavelet_level=3, max_wavelet_level=10, erosion_times=5):
	imArray = cv2.imread(image_path)
	#trim the image to 1200*1400
	imArray = imArray[0:1200,0:1400]
	#transform to grayscale
	imArray = cv2.cvtColor(imArray, cv2.COLOR_BGR2GRAY)
	#transform to float (0~1)
	imArray =  np.float32(imArray)   
	imArray /= 255
	#calculate wavelet coefficients (Haar base)
	mode = "haar"
	coeffs=pywt.wavedec2(imArray, mode, level=10)
	#abandon coefficients of specified levels
	coeffs_H=list(coeffs)
	if 0 < min_wavelet_level:
		coeffs_H[0] *= 0
	for i in range(11):
		if (i < min_wavelet_level or i > max_wavelet_level):
			coeffs_H[i] = tuple([np.zeros_like(v) for v in coeffs_H[i]])
	#reconstruct the image
	imArray_H=pywt.waverec2(coeffs_H, mode)
	imArray_H *= 255
	imArray_H =  np.uint8(imArray_H)
	#binarize the image using Otsu's method
	_,thr = cv2.threshold(imArray_H,0,255,cv2.THRESH_BINARY+cv2.THRESH_OTSU)
	#morphological operations
	#set the kernel
	kernel = cv2.getStructuringElement(cv2.MORPH_CROSS,(3,3))
	#erode the white region several times
	binary_image = cv2.erode(thr, kernel, iterations = erosion_times)
	#get coordinates of white pixels
	y,x = binary_image.nonzero()
	white_pixels = np.array([x,y])
	white_pixels = white_pixels.T
	#output
	np.savetxt(coordinate_data_path, white_pixels,fmt="%.0f",delimiter=",")

preprocess(image_path, coordinate_data_path,min_wavelet_level, max_wavelet_level, erosion_times)
subprocess.call("Rscript " + R_script_path + " " + coordinate_data_path + " " + output0 + " " +  output1, shell = True)
