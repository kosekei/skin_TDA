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

intermediate_dir = "data/homcloud_binary_intermediate/"
output = "data/count_binary.txt"


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
	#pd_max_min.to_csv(output, sep='\t')

pd_max_min_0dim = get_max_min(intermediate_dir, 0)
pd_max_min_1dim = get_max_min(intermediate_dir, 0)
print(pd_max_min_0dim)
print(pd_max_min_1dim)

colnames = pd.Series(
["comp_" + str(x) + "_" + str(y) for y in np.arange(20, 0, -1) for x in np.arange(0, 20, 1)]
+["hole_" + str(x) + "_" + str(y) for y in np.arange(20, 0, -1) for x in np.arange(0, 20, 1)]
)

with open(output, mode="w") as fo:
	fo.write("ID\ttime")
	for item in colnames:
		fo.write("\t"+item)
	fo.write("\n")


def count_diag(diag, max_midlife, min_midlife, max_lifetime, nx=20, ny=20):
	dx = (max_midlife - min_midlife) / nx
	dy = max_lifetime / ny
	x_lower = np.linspace(min_midlife, max_midlife-dx, nx)
	y_lower = np.linspace(0, max_lifetime-dy, ny)
	img = np.zeros((ny, nx))
	for j in range(ny):
		dj = diag[ (diag["lifetime"] <= y_lower[j] + dy).values & (diag["lifetime"] >= y_lower[j]).values ]
		for k in range(nx):
			djk = dj[ (dj["midlife"] >= x_lower[k]).values & (dj["midlife"] <= x_lower[k]+dx).values ]
			img[j,k] = len(djk)
	return(img)

for num in range(1,133):
	num = "{0:03d}".format(num)
	ID = "A-"+ str(num)
	for i in range(1,7):
		filename0 = intermediate_dir + ID + "-" + str(i) + "_binary_diag0.txt"
		filename1 = intermediate_dir + ID + "-" + str(i) + "_binary_diag1.txt"
		print(ID + "-" + str(i))
		try:
			diag0 = pd.read_table(filename0)
			img0 = count_diag(diag0, max_midlife = pd_max_min_0dim["max"]["midlife"] , min_midlife = pd_max_min_0dim["min"]["midlife"], max_lifetime = pd_max_min_0dim["max"]["lifetime"], nx=20, ny=20)
			img_flt0 = np.ravel(img0)
			diag1 = pd.read_table(filename1)
			img1 = count_diag(diag1, max_midlife = pd_max_min_1dim["max"]["midlife"] , min_midlife = pd_max_min_1dim["min"]["midlife"], max_lifetime = pd_max_min_1dim["max"]["lifetime"], nx=20, ny=20)
			img_flt1 = np.ravel(img1)
			img_flt = np.append(img_flt0,img_flt1)
			with open(output, mode="a") as fo:
				fo.write(str(ID)+"\t"+str(i)+"\t")
				for item in img_flt:
					fo.write(str(item)+"\t")
				fo.write("\n")
		except:
			pass

for num in range(1,114):
	num = "{0:03d}".format(num)
	ID = "D-"+ str(num)
	for i in range(1,7):
		filename0 = intermediate_dir + ID + "-" + str(i) + "_binary_diag0.txt"
		filename1 = intermediate_dir + ID + "-" + str(i) + "_binary_diag1.txt"
		print(ID + "-" + str(i))
		try:
			diag0 = pd.read_table(filename0)
			img0 = count_diag(diag0, max_midlife = pd_max_min_0dim["max"]["midlife"] , min_midlife = pd_max_min_0dim["min"]["midlife"], max_lifetime = pd_max_min_0dim["max"]["lifetime"], nx=20, ny=20)
			img_flt0 = np.ravel(img0)
			diag1 = pd.read_table(filename1)
			img1 = count_diag(diag1, max_midlife = pd_max_min_1dim["max"]["midlife"] , min_midlife = pd_max_min_1dim["min"]["midlife"], max_lifetime = pd_max_min_1dim["max"]["lifetime"], nx=20, ny=20)
			img_flt1 = np.ravel(img1)
			img_flt = np.append(img_flt0,img_flt1)
			with open(output, mode="a") as fo:
				fo.write(str(ID)+"\t"+str(i)+"\t")
				for item in img_flt:
					fo.write(str(item)+"\t")
				fo.write("\n")
		except:
			pass
