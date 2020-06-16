from itertools import product

import numpy as np
import pandas as pd
from sklearn import datasets
from scipy.stats import multivariate_normal as mvn
import matplotlib.pyplot as plt
import pywt,cv2,sys,os,subprocess,glob
import glob, shutil
import collections
from scipy.stats import norm
import scipy.spatial as spatial
import matplotlib.pyplot as plt
from sklearn.base import TransformerMixin

intermediate_dir = "data/homcloud_binary_intermediate/" #中間ファイル置き場


def get_max_min(intermediate_dir, dim):
	pd_max = pd.DataFrame()
	pd_min = pd.DataFrame()
	for diag in glob.glob(intermediate_dir + "/*diag"+str(dim)+"*"):
		try:
			d = pd.read_table(diag)
			dmax = d.apply(np.max)
			pd_max = pd.concat([pd_max, dmax], axis=1)
			dmin = d.apply(np.min)
			pd_min = pd.concat([pd_min, dmin], axis=1)
		except:
			pass	
	pd_max = pd_max.T
	pd_max = pd_max.apply(np.max)
	pd_min = pd_min.T
	pd_min = pd_min.apply(np.min)
	pd_max_min = pd.concat([pd_max, pd_min], axis=1)
	pd_max_min.columns = ["max","min"]
	return(pd_max_min)


pd_max_min_0dim = get_max_min(intermediate_dir, 0)
pd_max_min_1dim = get_max_min(intermediate_dir, 1)
print(pd_max_min_0dim)
print(pd_max_min_1dim)


def transform(diagram, max_midlife, min_midlife, max_lifetime, nx=20, ny=20, sd=0.1):
	diag_array = diagram[["midlife","lifetime"]].values
	dx = (max_midlife - min_midlife) / nx
	dy = max_lifetime / ny
	xs_lower = np.linspace(min_midlife, max_midlife-dx, nx)
	xs_upper = np.linspace(min_midlife, max_midlife-dx, nx) + dx
	ys_lower = np.linspace(0, max_lifetime-dy, ny)
	ys_upper = np.linspace(0, max_lifetime-dy, ny) + dy
	img = np.zeros((nx, ny))
	for point in diag_array:
	    x_smooth = norm.cdf(xs_upper, point[0], sd) - norm.cdf(xs_lower, point[0], sd)
	    y_smooth = norm.cdf(ys_upper, point[1], sd) - norm.cdf(ys_lower, point[1], sd)
	    img += np.outer(x_smooth, y_smooth) * (point[1] /max_lifetime) 
	img = img.T[::-1]
	return(img)

filename0 = intermediate_dir + "A-061-4_binary_diag0.txt"
diag0 = pd.read_table(filename0)
img0 = transform(diag0, max_midlife = pd_max_min_0dim["max"]["midlife"] , min_midlife = pd_max_min_0dim["min"]["midlife"], max_lifetime = pd_max_min_0dim["max"]["lifetime"], nx=20, ny=20, sd=0.1)
ar0 = np.zeros((400,3))
for i in range(20):
	for j in range(20):
		ar0[(20*i+j),0] = i
		ar0[(20*i+j),1] = j
		ar0[(20*i+j),2] = img0[i,j]
np.savetxt("data/A-061-4_persim_0dim.txt", ar0, delimiter="\t")

filename1 = intermediate_dir + "A-061-4_binary_diag1.txt"
diag1 = pd.read_table(filename1)
img1 = transform(diag1, max_midlife = pd_max_min_1dim["max"]["midlife"] , min_midlife = pd_max_min_1dim["min"]["midlife"], max_lifetime = pd_max_min_1dim["max"]["lifetime"], nx=20, ny=20, sd=0.1)
ar1 = np.zeros((400,3))
for i in range(20):
	for j in range(20):
		ar1[(20*i+j),0] = i
		ar1[(20*i+j),1] = j
		ar1[(20*i+j),2] = img1[i,j]
np.savetxt("data/A-061-4_persim_1dim.txt", ar1, delimiter="\t")
