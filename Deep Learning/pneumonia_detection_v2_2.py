# -*- coding: utf-8 -*-
"""pneumonia_detection-V2_2.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1QnO0DfaS7DAzxI6qOZYoEhoQZ4inXepS

# Import necessary libraries
"""

# Commented out IPython magic to ensure Python compatibility.
# %matplotlib inline
import tensorflow as tf
import matplotlib.pyplot as plt
import numpy as np
import os
import PIL
import PIL.Image
import tensorflow_datasets as tfds
import pathlib

# Set the default figure size for matplotlib
plt.rcParams['figure.figsize'] = (7,7) # Make the figures a bit bigger

"""# Load data and preprocess it

## Option 1:
Download the dataset directly from the provided link. If this does not work, try Option **2**
"""

dataset_url = 'http://vision.roboslang.org/open_datasets/pneumonia_dataset.zip'

"""## Option 2:
If downloading the dataset from the link is too slow or the link is no longer working, download the dataset from Brigthspace, and upload it to the sample_data folder in Google Colab. For this, open the Files in the left hand side menu, and click the upload button, or drag and drop the archive in the folder you want. Depending on where you have copied this file, you might have to change the path below.

When you upload from the interface sometimes you don't get a lot of feedback for when the upload is done. You can also try to force the upload window to open by executing the below code.
"""

from google.colab import files
files.upload()

# For option 2, after you have copied the data locally, point the dataset_url to the local path

dataset_url = '/content/sample_data/datasets/pneumonia_dataset.zip'

"""##Preprocess the data"""

out_path = '/content/sample_data/'
archive = tf.keras.utils.get_file(origin=dataset_url, cache_dir='/content/sample_data/', extract=True)

# Create a 'pathlib.Path' object for the downloaded archive
# Pathlib module offers classes representing filesystem paths with semantics
# appropriate for different operating systems.
data_dir = pathlib.Path(archive).with_suffix('')

# Count the number of images in a specific directory
image_count = len(list(data_dir.glob('./train/pneumonia/*.jpeg')))
print(image_count)

# Pneumonia dataset is split into train and test folders. Inside those folders you will
# find additional folders: pneumonia and normal. You can explore the folders using
# 'Files' tab from the right hand side.
# How many images do we have in training for pneumonia? How many for normal CT scans?
# How about in the testing set? How many for pneumonia and how many for normal scans?
# Tip: use the len() function

# print(data_dir)
normtrain = len(list(data_dir.glob('./train/normal/*.jpeg')))
print("normal training set -", normtrain)
pntrain = len(list(data_dir.glob('./train/pneumonia/*.jpeg')))
print("pneumonia training set -", pntrain)

normtest = len(list(data_dir.glob('./test/pneumonia/*.jpeg')))
print("normal test set -", normtest)
pntest = len(list(data_dir.glob('./test/pneumonia/*.jpeg')))
print("pneumonia test set -", pntest)

# 3. Display a pneumonia and a normal image from the testing dataset

# Create a list of file paths for pneumonia images
pneumonia_images = list(data_dir.glob('train/pneumonia/*'))
# Open and display the first pneumonia image in the list
PIL.Image.open(str(pneumonia_images[0]))

# Create a list of file paths for normal images
normal_images = list(data_dir.glob('train/normal/*'))
# Open and display the first normal image in the list
PIL.Image.open(str(normal_images[0]))

"""# Define a deep learning model that will learn the differences between pneumonia and normal CT images

"""

# Define batch size and image dimensions for training

# The batch size is the number of samples processed before the model is updated.
# Choose an appropriate batch size. Find out the resolution of the image and
# fill in the values of the following three variables
batch_size = 10
img_height = 360
img_width = 360

train_data_dir  = os.path.join(data_dir,'train')
test_data_dir = os.path.join(data_dir,'test')

# Create a TensorFlow image dataset from a directory
# Use the function tf.keras.utils.image_dataset_from_directory in order to load
# the training dataset: https://www.tensorflow.org/api_docs/python/tf/keras/utils/image_dataset_from_directory
# 1. First argument is your training directory folder,
# 2. Use 20% of the data for validation, for image size,
# 3. name the subset as "training"
# 4. you can set a seed such that when you repeat experiments you get similar results, eg: seed=123
# 5. for image size use the img_height and img_width variables you defined previously.
# 6. Use for batch size the batch_size variable you defined earlier in the code
train_ds = tf.keras.utils.image_dataset_from_directory(
 train_data_dir,
 validation_split=0.2,
 subset='training',
 seed=100,
 image_size=(img_height, img_width),
 batch_size=batch_size
)

# Create a layer to normalise pixel values to the [0, 1] range.
# By default, when you load an image, each pixel value will have a value between 0-255
# but, in neural networks, we need as input normalised values in [0,1] interval.
normalization_layer = tf.keras.layers.Rescaling(1./255)

normalized_ds = train_ds.map(lambda x, y: (normalization_layer(x), y))
image_batch, labels_batch = next(iter(normalized_ds))
first_image = image_batch[0]
# Notice the pixel values are now in `[0,1]`.
print(np.min(first_image), np.max(first_image), first_image[0][0])

# Define the number of classes in the classification problem.
# How many classes do we have in this dataset?
num_classes = 2

from keras.src.layers.serialization import activation
# Define a tensorflow model using the tf.keras.Sequential class: https://www.tensorflow.org/api_docs/python/tf/keras/Sequential
# The last layer should be a Dense layer with the number of output neurons num_classes
# Use as a starting point the tf.keras.Sequential model defined for the MNIST problem.
# See Lab-DeepLearning-ImageClassification.
# For the first Conv2D layer, you are not required to specify the input shape. If that
# parameter is not given, tensorflow library will infer the size of the input when
# you fit the model, so it will depend on the size of the dataset.
# Important: change the output of the last Dense layer to match the number of classes for this problem.
# If you don't use any of the Dropout layers what accuracy do you get?
# What accuracy do you get with the Dropout layers?
model = tf.keras.Sequential([
  tf.keras.layers.Input(shape=(img_height, img_width, 3)),
  tf.keras.layers.Conv2D(16, (3,3), activation='relu'), # numbers 16, 32, 64 represent no filters
  tf.keras.layers.MaxPooling2D((2, 2)),
  tf.keras.layers.Conv2D(32, (3,3), activation="relu"),
  tf.keras.layers.MaxPooling2D((2, 2)),
  tf.keras.layers.Conv2D(64, (3,3), activation="relu"),
  tf.keras.layers.MaxPooling2D((2, 2)),
  tf.keras.layers.Flatten(),
  tf.keras.layers.Dense(128, activation="relu"),
  tf.keras.layers.Dense(num_classes, activation=tf.nn.softmax), #softmax ensures the scores max sum to 1
])

# Compile the model with an optimizer, loss function, and evaluation metric
# There are several ways in which the loss or error between the label and
# the predictions can be computed. One of them is called SparseCategoricalCrossentropy
# https://www.tensorflow.org/api_docs/python/tf/keras/losses/SparseCategoricalCrossentropy
# You can also experiment with BinaryCrossentropy since for this problem we also have
# two classes, pneumonia vs normal: https://www.tensorflow.org/api_docs/python/tf/keras/losses/BinaryCrossentropy
model.compile(
  optimizer='adam',
  #loss=tf.keras.losses.BinaryCrossentropy(from_logits=False), - using this loss function caps accuracy at 50
  loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True),
  metrics=['accuracy']) # Monitor accuracy and F1 score during training

# Train the model on the provided dataset for a specified number of epochs
# Modify the network architecture such that you maximise the accuracy.
# Tip: aim to get an accuracy of at least 90% on the training set.
# For this, you can use the function fit, as in model.fit(...)
# The first argument is the train_ds variable defined above.
# This variable contains both the x (data - pneumonia and normal images) and y
# (labels - pneumonia vs normal).
# Start training using 5 epochs. What is the accuracy you get?
# How about if you increase the number of epochs?
model.fit(train_ds, epochs=15)

# Let's load the testing dataset
test_ds = tf.keras.utils.image_dataset_from_directory(
  test_data_dir,
  image_size=(img_height, img_width),
  batch_size=batch_size)

# What is the loss and accuracy on the Testing dataset?
# Tip: instead of (x_test, y_test) we used in the lab last week, you can use
# directly test_ds which contains both data and labels
# https://www.tensorflow.org/api_docs/python/tf/keras/Model#evaluate
# eg: model.evaluate(...)
# You only need to specify the testing dataset.
print("Model accuracy on the test set is:", model.evaluate(test_ds))

# Try to improve the model such that it performs well on both training and testing datasets.