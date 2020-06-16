#execute: python3 script_path image_path min_wavelet_level max_wavelet_level erosion_times output0 output1

import numpy as np
import pandas as pd
import pywt,cv2,sys,subprocess,homcloud,os
import matplotlib.pyplot as plt
import homcloud.interface as hc

args = sys.argv
image_path = args[1] # jpg file
min_wavelet_level = args[2] #int
max_wavelet_level = args[3] #int
erosion_times = args[4] #int
output0 = args[5] #txt file
output1 = args[6] #txt file

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

def homcloud_binary(binary_image, output0, output1):
	#get the locations of white pixels
	white_region = binary_image > 128
	#execute filtration
	diag = hc.PDList.from_bitmap_levelset(hc.distance_transform(white_region, signed=True))
	#get the 0-dim persistence diagram
	p0 = diag.dth_diagram(0)
	#calculate mid-life and life-time
	p0_diag = np.vstack([p0.births, p0.deaths, (p0.births+p0.deaths)/2, np.abs(p0.births-p0.deaths)]).transpose()
	p0_df = pd.DataFrame(p0_diag, columns=["birth","death","midlife", "lifetime"])
	#output
	p0_df.to_csv(path_or_buf=output0, sep="\t", index=False)
	#get the 1-dim persistence diagram
	p1 = diag.dth_diagram(1)
	#get the 1-dim persistence diagram	
	p1_diag = np.vstack([p1.births, p1.deaths, (p1.births+p1.deaths)/2, np.abs(p1.births-p1.deaths)]).transpose()
	#calculate mid-life and life-time
	p1_df = pd.DataFrame(p1_diag, columns=["birth","death","midlife", "lifetime"])
	#output
	p1_df.to_csv(path_or_buf=output1, sep="\t", index=False)

binary_image = preprocess(image_path, min_wavelet_level, max_wavelet_level, erosion_times)
homcloud_binary(binary_image, output0, output1)