"""Small CNN for MNIST in Keras (Chapter 24).

Tested in the course hls4ml environment (Keras 2.11); Keras 3 compatible? check.
keras.datasets.mnist downloads the data on first use.
"""
import keras
from keras import layers
from keras.datasets import mnist

keras.utils.set_random_seed(0)      # repeatable start (Keras 2.11 and Keras 3)

(x_tr, y_tr), (x_te, y_te) = mnist.load_data()
x_tr = x_tr[..., None] / 255.0                  # (N, 28, 28, 1), 0..1
x_te = x_te[..., None] / 255.0

model = keras.Sequential([
    keras.Input(shape=(28, 28, 1)),
    layers.Conv2D(32, 3, activation="relu"),  # 320 w
    layers.MaxPooling2D(2),          # 13x13x32
    layers.Flatten(),
    layers.Dense(10, activation="softmax"),
])
model.compile(optimizer="adam", loss="sparse_categorical_crossentropy",
              metrics=["accuracy"])
model.summary()
model.fit(x_tr, y_tr, batch_size=128, epochs=3, validation_split=0.1)  # OK: MNIST is not sorted
print(model.evaluate(x_te, y_te))
