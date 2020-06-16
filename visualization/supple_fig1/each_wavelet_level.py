import numpy as np
import pywt,cv2,sys, subprocess
import matplotlib.pyplot as plt
import os.path

imArray = cv2.imread(image_path)

def preprocess(image_path, min_wavelet_level, max_wavelet_level, erosion_times):
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
	return(binary_image)

for i in range(11):
	binary_image = preprocess(image_path, min_wavelet_level=i, max_wavelet_level=i, erosion_times=5)
	cv2.imwrite("data/level" + str(i) + ".png", binary_image)