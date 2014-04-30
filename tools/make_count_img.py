#!/usr/bin/python

import cv2
import numpy

w = 16
h = 16
d = 1

img = numpy.zeros((w,h,d),dtype=numpy.uint8)

for i in range(h):
  for j in range(w):
    pixel = ((i&0xF)<<4) + (j&0xF)
    img[i,j,:] = d*[pixel]

cv2.imwrite('count.ras',img)
