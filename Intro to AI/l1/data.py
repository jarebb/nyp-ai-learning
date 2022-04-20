import numpy as np
import sklearn.preprocessing

input_data = np.array([[1.5, -2.0, 5.5],
[-2.5, 2.4, 4.5],
[0.5, -8.0, 5.0],
[6.9, 2.0, -5.2]])

binarizer = sklearn.preprocessing.Binarizer(threshold = 0.5)
data_binarized = binarizer.transform(input_data)
print("\nBinarized data:\n", data_binarized)
