#execute: python3 script_path image_path min_wavelet_level max_wavelet_level output0 output1

import numpy as np
import pandas as pd
import pywt,cv2,sys,subprocess,homcloud,os
import matplotlib.pyplot as plt
import homcloud.interface as hc

args = sys.argv
image_path = args[1] #jpg file
min_wavelet_level = args[2] #int
max_wavelet_level = args[3] #int
output0 = args[4] #txt file
output1 = args[5] #txt file

def preprocess(image_path, min_wavelet_level=3, max_wavelet_level=10):
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
	gray_image =  np.uint8(imArray_H)
	return(gray_image)

def homcloud_gray(gray_image, output0, output1):
	#get the locations of white pixels
	white_region = gray_image > 128
	#execute filtration
	diag = hc.PDList.from_bitmap_levelset(gray_image, "superlevel")
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

gray_image = preprocess(image_path, min_wavelet_level, max_wavelet_level)
homcloud_gray(gray_image, output0, output1)
