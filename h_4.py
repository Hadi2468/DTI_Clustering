#!/usr/bin/env python
# coding: utf-8

print("    Version control\n------------------------")
import os, fnmatch, random, datetime, math, sys
from pathlib import Path
import numpy as np;              print("Numpy\t\t", np.__version__)
import matplotlib as mpl;        print("matplotlib\t", mpl.__version__)
import matplotlib.pyplot as plt
import nibabel as nib;           print("NiBabel\t\t {}".format(nib.__version__))
from nibabel.testing import data_path
import pandas as pd;             print("Pandas\t\t {}".format(pd.__version__))
import imageio;                  print("imageio\t\t {}".format(imageio.__version__))
import h5py;                     print("H5py\t\t {}".format(h5py.__version__))
import sklearn;                  print("Scikit-learn\t {}".format(sklearn.__version__))
import skimage;                  print("Scikit-image\t {}".format(skimage.__version__))
import tensorflow as tf;         print("TensorFlow\t {}".format(tf.__version__))
import keras as K;               print("Keras\t\t {}".format(K.__version__))
from tensorflow.keras import models, Input, Model
from tensorflow.keras.layers import Dense, Flatten, Reshape, BatchNormalization, Conv3D, MaxPooling3D, UpSampling3D
from tensorflow.keras.activations import relu, sigmoid
from tensorflow.keras.losses import SparseCategoricalCrossentropy
from tensorflow.keras.initializers import *
from keras.callbacks import TensorBoard, EarlyStopping

## Loading dataset from path directory
print(os.getcwd())     # '/Users/shossein/GitHub/DTI_Clustering'

## load train data
sample_train_subset = np.loadtxt("sample_train_1.csv", dtype=str, delimiter=",")                                     # Change
train_data = np.load('train_1.npy').reshape(100,182,218,182,1)       	       	       	       	                     # Change
print('train_data shape is {}'.format(train_data.shape))

## load validation data
sample_val_subset = np.loadtxt("sample_valid_1.csv", dtype=str, delimiter=",")           	       	       	     # Change
valid_data = np.load('valid_1.npy').reshape(24,182,218,182,1)       	       	       	       	                     # Change
print('valid_data shape is {}'.format(valid_data.shape))

## Define parameters:
IMAGE_HEIGHT = train_data.shape[1]
IMAGE_WIDTH = train_data.shape[2]
IMAGE_DEPTH = train_data.shape[3]

batch_size = 1                                                                                                       # Change

data_shape = [1, IMAGE_HEIGHT, IMAGE_WIDTH, IMAGE_DEPTH, 1]
input_shape = [batch_size, IMAGE_HEIGHT, IMAGE_WIDTH, IMAGE_DEPTH, 1]
print("input-layer shape:", input_shape)

## Encoder
input_img = Input(shape=(182, 218, 182, 1))
x = Conv3D(filters=128, kernel_size=3, strides=2, padding='same', activation='relu', name='Conv1')(input_img)          # Change
x = BatchNormalization()(x)

x = Conv3D(filters=64, kernel_size=3, strides=2, padding='same', activation='relu', name='Conv2')(x)                  # Change
x = BatchNormalization()(x)

x = Conv3D(filters=32, kernel_size=3, strides=2, padding='same', activation='relu', name='Conv3')(x)                  # Change
x = BatchNormalization()(x)


## Latent Features
shape_before_flattening = tf.keras.backend.int_shape(x)
x = Flatten()(x)
init = VarianceScaling(scale=1. / 3., mode='fan_in', distribution='uniform')
## encoded = Dense(4, kernel_initializer=init, activation='relu', name='encoded')(x)
encoded = Dense(4, activation='relu', name='encoded')(x)
x = BatchNormalization()(encoded)
## x = Dense(np.prod(shape_before_flattening[1:]), activation='relu', kernel_initializer=init)(encoded)
x = Dense(np.prod(shape_before_flattening[1:]), activation='relu')(encoded)
x = Reshape(shape_before_flattening[1:])(x)

## Decoder
x = Conv3D(filters=32, kernel_size=3, padding='same', activation='relu', name='DeConv1')(x)                           # Change
x = BatchNormalization()(x)
x = UpSampling3D(size=(2, 2, 2))(x)

x = Conv3D(filters=64, kernel_size=3, padding='same', activation='relu', name='DeConv2')(x)                           # Change
x = BatchNormalization()(x)
x = UpSampling3D(size=(2, 2, 2))(x)

x = Conv3D(filters=128, kernel_size=(2,4,2), padding='valid', activation='relu', name='DeConv3')(x)                    # Change
x = BatchNormalization()(x)
x = UpSampling3D(size=(2, 2, 2))(x)

decoded = Conv3D(filters=1, kernel_size=3, padding='same', activation='sigmoid', name='Output')(x)  

model_CAE = Model(inputs=input_img, outputs=decoded)
model_CAE.compile(optimizer='adam', loss='mse', metrics=['accuracy'])
model_CAE.summary()

## Tensorboard
logdir = os.path.join("CAE_logs_128", datetime.datetime.now().strftime("%Y_%m_%d____%H_%M_%S"))                        # Change
tb_callback = TensorBoard(logdir, histogram_freq=1)

## Start time:
from datetime import datetime
start_time = datetime.now().strftime("%H:%M:%S")
print("\nStart Time =", start_time, "\n")

## Midel Fit
# early_stopping = EarlyStopping(monitor='val_loss', min_delta=0, patience=10, verbose=5, mode='auto')
model_CAE.fit(train_data, train_data, epochs=100, batch_size=batch_size, shuffle=True, validation_data=(valid_data, valid_data), callbacks=[tb_callback], verbose=1)     # Change 
model_CAE.save_weights("CAE_weights_128.hdf5")                                                                          # Change

# End time:
from datetime import datetime
end_time = datetime.now().strftime("%H:%M:%S")
print("\nEnd Time =", end_time)

model_CAE.load_weights("CAE_weights_128.hdf5")                                                                          # Change
test_data = train_data[0,:].reshape(1, 182, 218, 182, 1)
reconstructed = model_CAE.predict(test_data)

print('\ntrain_data[0,100,100:110,100]\n\n {}'.format(train_data[0,100,100:110,100]),'\n')
print('\nReconstructed_data[0,100,100:110,100]\n\n {}'.format(reconstructed[0,100,100:110,100]),'\n')

h5_file = h5py.File('CAE_weights_128.hdf5', 'r')                                                                        # Change
print(list(h5_file.keys()))
# for L in (list(h5_file.keys())):
#     print(L)
#     L = h5_file[layer]
#     W = L[layer]['kernel:0']
#     print(W.shape)

L = h5_file[list(h5_file.keys())[0]]
W = L[list(h5_file.keys())[0]]['kernel:0']
print('\nWeights shape: {}'.format(W.shape))
print('\nWeights[1][1][1]: {}'.format(W[1][1][1]))

