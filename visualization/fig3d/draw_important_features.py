import numpy as np
import pandas as pd
import pywt,cv2,sys,subprocess,homcloud,os
import matplotlib.pyplot as plt
import homcloud.interface as hc

imArray = cv2.imread(image_path)
imArray_trimmed = imArray[0:1200,0:1400]
imArray_gray = cv2.cvtColor(imArray_trimmed, cv2.COLOR_BGR2GRAY)
imArray_gray =  np.float32(imArray_gray)   
imArray_gray /= 255
mode = "haar"
coeffs=pywt.wavedec2(imArray_gray, mode, level=10)
coeffs_H[0] *= 0
imArray_H=pywt.waverec2(coeffs_H, mode)
imArray_H *= 255
imArray_H =  np.uint8(imArray_H)
_,binary_image = cv2.threshold(imArray_H,0,255,cv2.THRESH_BINARY+cv2.THRESH_OTSU)

binary_image = binary_image > 128
pdiag = hc.PDList.from_bitmap_levelset(hc.distance_transform(binary_image, signed=True))

p0 = pdiag.dth_diagram(0)
pairs0 = p0.pairs()
pairs0_imp_1 = [pair for pair in pairs0 if (pair.birth_time() + pair.death_time()) >= -2.125 and (pair.birth_time() + pair.death_time()) <= (-2.125 + 8.125) and (pair.death_time() - pair.birth_time()) >=0 and (pair.death_time() - pair.birth_time()) <=(0+4.45)]

p1 = pdiag.dth_diagram(1)
pairs1 = p1.pairs()
pairs1_imp_1 = [pair for pair in pairs1 if (pair.birth_time() + pair.death_time()) >= 1.4 and (pair.birth_time() + pair.death_time()) <= (1.4 + 2.175) and (pair.death_time() - pair.birth_time()) >=0 and (pair.death_time() - pair.birth_time()) <=(0+1.8)]

birth_pixels_image0 = hc.draw_birthdeath_pixels_2d(
    pairs=pairs0_imp_1, image=imArray_trimmed, draw_birth=True, 
    birth_color=(255, 0, 34),marker_size=5
)
birth_pixels_image1 = hc.draw_birthdeath_pixels_2d(
    pairs=pairs1_imp_1, image=birth_pixels_image0, draw_birth=False, draw_death=True, death_color=(0, 200, 255), marker_size=5
)

fig = plt.figure()
plt.imshow(birth_pixels_image1)
plt.savefig(output, dpi=300)

im_new = birth_pixels_image1.crop((500,400,900,800))

fig = plt.figure()
plt.imshow(im_new)
plt.savefig(output_enlarged, dpi=600)
