#!/usr/bin/env python
# coding: utf-8

print("\nVersion control:\n")
import os     # operating system interfaces
import fnmatch
import random
import datetime
import numpy as np; print("Numpy\t\t", np.__version__)
import matplotlib as mpl; print("matplotlib\t", mpl.__version__)
import matplotlib.pyplot as plt
import nibabel as nib; print("NiBabel\t\t", nib.__version__)
from nibabel.testing import data_path
import math
import pandas as pd; print("Pandas\t\t", pd.__version__)
import sys
import imageio; print("imageio\t\t", imageio.__version__)
import h5py; print("H5py\t\t", h5py.__version__)
import sklearn; print("Scikit-learn\t", sklearn.__version__)
import skimage; print("Scikit-image\t", skimage.__version__)
import tensorflow as tf; print("TensorFlow\t", tf.__version__)
import keras; print("Keras\t\t", keras.__version__)
from tensorflow.keras import models, Input, Model
from tensorflow.keras.layers import Dense, Conv3D, Conv3DTranspose, MaxPooling3D, UpSampling3D
from tensorflow.keras.activations import relu, sigmoid
from tensorflow.keras.losses import SparseCategoricalCrossentropy
from keras.callbacks import TensorBoard

train_data_loaded = np.random.normal(0.5, 0.05, (5, 182, 218, 182)).reshape((5, 182, 218, 182, 1))
valid_data_loaded = np.random.normal(0.5, 0.05, (3, 182, 218, 182)).reshape((3, 182, 218, 182, 1))
arr0 = np.min(train_data_loaded)
arr1 = np.max(train_data_loaded)
arr2 = np.min(valid_data_loaded)
arr3 = np.max(valid_data_loaded)
print('\nShape of Train and Valid datasets are', train_data_loaded.shape, valid_data_loaded.shape)
print('Minimum and Maximum value of Train and Valid datasets are', arr0, arr1, arr2, arr3)
print('element of [1, 100, 100 ,100, 1] is', train_data_loaded[[1],[100],[100],[100]], valid_data_loaded[[1],[100],[100],[100]], '\n')

## Define parameters:

IMAGE_HEIGHT = train_data_loaded.shape[1]
IMAGE_WIDTH = train_data_loaded.shape[2]
IMAGE_DEPTH = train_data_loaded.shape[3]
batch_size = 32
data_shape = [1, IMAGE_HEIGHT, IMAGE_WIDTH, IMAGE_DEPTH, 1]
input_shape = [batch_size, IMAGE_HEIGHT, IMAGE_WIDTH, IMAGE_DEPTH, 1]
# print("input-layer shape:", input_shape)

## Encoder
input_img = Input(shape=(182, 218, 182, 1))
x = Conv3D(filters=16, kernel_size=(3, 3, 3), padding='same', activation='relu', name='Conv1')(input_img)
x = MaxPooling3D(pool_size=(13, 13, 13), padding='same')(x)
x = Conv3D(filters=8, kernel_size=(3, 3, 3), padding='same', activation='relu', name='Conv2')(x)
encoded = MaxPooling3D(pool_size=(7, 7, 7), padding='same')(x)
## at this point the representation is (2, 3, 2, 8) i.e. 96-dimensional instead of 7,221,032

## Decoder
x = Conv3DTranspose(filters=4, kernel_size=(3, 3, 3), padding='same', activation='relu', name='DeConv3')(encoded)
x = UpSampling3D(size=(7, 6, 7))(x)
x = Conv3DTranspose(filters=8, kernel_size=(3, 3, 3), padding='same', activation='relu', name='DeConv4')(x)
x = UpSampling3D(size=(13, 12, 13))(x)
decoded = Conv3DTranspose(filters=1, kernel_size=(1, 3, 1), padding='valid', activation='sigmoid', name='DeConv5')(x)

autoencoder = Model(inputs=input_img, outputs=decoded)
autoencoder.compile(optimizer='adam', loss=SparseCategoricalCrossentropy(from_logits=False), metrics=['accuracy'])
# autoencoder.summary()

autoencoder.fit(train_data_loaded, train_data_loaded, epochs=3, batch_size=batch_size, shuffle=True, validation_data=(valid_data_loaded, valid_data_loaded), verbose=1)

