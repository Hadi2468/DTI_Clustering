#!/usr/bin/env python
# coding: utf-8

print("    Version control\n------------------------")
import os, fnmatch, random, math, sys, datetime
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
get_ipython().run_line_magic('load_ext', 'tensorboard')

## Load train data
sample_train_subset = np.loadtxt("sample_train_1.csv", dtype=str, delimiter=",")
train_data = np.load('train_1.npy').reshape(100,182,218,182,1)
print('train_data shape is {}'.format(train_data.shape))

## Load validation data
sample_val_subset = np.loadtxt("sample_valid_1.csv", dtype=str, delimiter=",")
valid_data = np.load('valid_1.npy').reshape(24,182,218,182,1)
print('valid_data shape is {}'.format(valid_data.shape))

## Define parameters:
IMAGE_HEIGHT = train_data.shape[1]
IMAGE_WIDTH = train_data.shape[2]
IMAGE_DEPTH = train_data.shape[3]
batch_size = 1
data_shape = [1, IMAGE_HEIGHT, IMAGE_WIDTH, IMAGE_DEPTH, 1]
input_shape = [batch_size, IMAGE_HEIGHT, IMAGE_WIDTH, IMAGE_DEPTH, 1]
print("input-layer shape:", input_shape)

## Encoder
input_img = Input(shape=(182, 218, 182, 1), name='Input')
x = Conv3D(filters=128, kernel_size=3, strides=2, padding='same', activation='relu', name='Conv1')(input_img)
x = BatchNormalization(name='BN_Conv1')(x)
x = Conv3D(filters=64, kernel_size=3, strides=2, padding='same', activation='relu', name='Conv2')(x)
x = BatchNormalization(name='BN_Conv2')(x)
x = Conv3D(filters=32, kernel_size=3, strides=2, padding='same', activation='relu', name='Conv3')(x)
x = BatchNormalization(name='BN_Conv3')(x)
x = Conv3D(filters=16, kernel_size=3, strides=2, padding='same', activation='relu', name='Conv4')(x)
x = BatchNormalization(name='BN_Conv4')(x)
x = Conv3D(filters=8, kernel_size=3, strides=2, padding='same', activation='relu', name='Conv5')(x)
x = BatchNormalization(name='BN_Conv5')(x)
x = Conv3D(filters=4, kernel_size=3, strides=2, padding='same', activation='relu', name='Conv6')(x)
x = BatchNormalization(name='BN_Conv6')(x)

## Latent Features
shape_before_flattening = tf.keras.backend.int_shape(x)
x = Flatten(name='Flat')(x)
# init = VarianceScaling(scale=1. / 3., mode='fan_in', distribution='uniform')
# encoded = Dense(50, kernel_initializer=init, activation='relu', name='encoded')(x)
# encoded = Dense(50, activation='relu', name='encoded')(x)
encoded = x
# x = BatchNormalization()(encoded)
# x = Dense(np.prod(shape_before_flattening[1:]), activation='relu', kernel_initializer=init)(encoded)
# x = Dense(np.prod(shape_before_flattening[1:]), activation='relu')(encoded)
x = Reshape(shape_before_flattening[1:], name='UnFlat')(x)

## Decoder
x = Conv3D(filters=4, kernel_size=3, padding='same', activation='relu', name='DeConv1')(x)
x = BatchNormalization(name='BN_DeConv1')(x)
x = UpSampling3D(size=(2, 2, 2), name='UpSampling1')(x)
x = Conv3D(filters=8, kernel_size=(1,2,1), padding='valid', activation='relu', name='DeConv2')(x)
x = BatchNormalization(name='BN_DeConv2')(x)
x = UpSampling3D(size=(2, 2, 2), name='UpSampling2')(x)
x = Conv3D(filters=16, kernel_size=3, padding='same', activation='relu', name='DeConv3')(x)
x = BatchNormalization(name='BN_DeConv3')(x)
x = UpSampling3D(size=(2, 2, 2), name='UpSampling3')(x)
x = Conv3D(filters=32, kernel_size=(2,1,2), padding='valid', activation='relu', name='DeConv4')(x)
x = BatchNormalization(name='BN_DeConv4')(x)
x = UpSampling3D(size=(2, 2, 2), name='UpSampling4')(x)
x = Conv3D(filters=64, kernel_size=(1,2,1), padding='valid', activation='relu', name='DeConv5')(x)
x = BatchNormalization(name='BN_DeConv5')(x)
x = UpSampling3D(size=(2, 2, 2), name='UpSampling5')(x)
x = Conv3D(filters=128, kernel_size=2, padding='valid', activation='relu', name='DeConv6')(x)
x = BatchNormalization(name='BN_DeConv6')(x)
x = UpSampling3D(size=(2, 2, 2), name='UpSampling6')(x)
decoded = Conv3D(filters=1, kernel_size=3, padding='same', activation='sigmoid', name='Output')(x)

model_CAE = Model(inputs=input_img, outputs=decoded)
## optimizer=rmsprop, sgd
model_CAE.compile(optimizer='adam', loss='mse', metrics=['accuracy'])
model_CAE.summary()

## Start time:
from datetime import datetime
start_time = datetime.now().strftime("%Y.%m.%d___%H:%M")
print("\nStart Time =", start_time, "\n")

## Loading last weights
# model_CAE.load_weights(os.path.join("Weights/Weights_L100___2020_12_30___03_43_23.hdf5"))

## Model Fit
tb_callback = TensorBoard(os.path.join("Logs/L100___" + start_time), histogram_freq=1)
# early_stopping = EarlyStopping(monitor='val_loss', min_delta=0, patience=10, verbose=5, mode='auto')
model_CAE.fit(train_data, train_data, validation_data=(valid_data, valid_data), epochs=1, batch_size=batch_size, shuffle=True, callbacks=[tb_callback], verbose=1)
model_CAE.save_weights(os.path.join("Weights/L100___" + start_time + ".hdf5"))

## End time:
from datetime import datetime
end_time = datetime.now().strftime("%Y_%m_%d____%H_%M")
print("\nEnd Time =", end_time)

test_data = train_data[0,:].reshape(1, 182, 218, 182, 1)
reconstructed = model_CAE.predict(test_data)

print('\ntrain_data[0,100,100:105,100]\n\n {}'.format(train_data[0,100,100:105,100]),'\n')
print('\nReconstructed_data[0,100,100:105,100]\n\n {}'.format(reconstructed[0,100,100:105,100]),'\n')

L = h5_file[list(h5_file.keys())[0]]
W = L[list(h5_file.keys())[0]]['kernel:0']
print('\nWeights shape: {}'.format(W.shape))
print('\nWeights[1][1][1]: {}'.format(W[1][1][1]))

# h5_file = h5py.File(os.path.join("Weights/L100___" + start_time + ".hdf5"), 'r')
h5_file = h5py.File(os.path.join("Weights/w_Pegasus_128.hdf5"), 'r')
Layer_size = len(list(h5_file.keys()))
Layer_names = list(h5_file.keys())
print("There are", Layer_size, "layers in this model as:\n\n", Layer_names,'\n')

for l in range(5):  #Layer_size
    print('==========================================================\n')
    layers = h5_file[Layer_names[l]]
#     print("Layer", l+1, "-----", layers)
    W = layers[Layer_names[l]]['kernel:0']
    print('Layer', l+1, ':', list(h5_file.keys())[l], '\tWeights\' shape: {}'.format(W.shape), '\n')
#     print('\nWeights[1][1][1]: {}'.format(W[1][1][1]))
    
    Kernel_1 = W.shape[0]
    Kernel_2 = W.shape[1]
    Kernel_3 = W.shape[2]
    Kernel_all = np.zeros([Kernel_1, Kernel_2, Kernel_3])
    for f in range(2):   # W.shape[4]
        for x in range(Kernel_1):
            for y in range(Kernel_2):
                for z in range(Kernel_3):
                    Kernel_all[x][y][z] = (W[x][y][z])[0][0]
        print('\nWeights of kernel', f+1, 'of', W.shape[4], ':\n\n', Kernel_all)

