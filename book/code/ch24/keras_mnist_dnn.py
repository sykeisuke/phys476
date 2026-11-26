"""Deep fully connected network on MNIST (Exercise 3). Tested in the course hls4ml
environment (Keras 2.11); Keras 3 compatible? check."""
import keras
from keras import layers
from keras.datasets import mnist

keras.utils.set_random_seed(0)      # repeatable start (Keras 2.11 and Keras 3)

(x_tr, y_tr), (x_te, y_te) = mnist.load_data()
x_tr = x_tr.reshape(-1, 784) / 255.0           # 28 x 28 pixels -> 784 numbers in 0..1
x_te = x_te.reshape(-1, 784) / 255.0

model = keras.Sequential([keras.Input(shape=(784,))]
                         + [layers.Dense(200, activation="relu") for _ in range(3)]
                         + [layers.Dense(10, activation="softmax")])
model.compile(optimizer="adam", loss="sparse_categorical_crossentropy",
              metrics=["accuracy"])
model.summary()
model.fit(x_tr, y_tr, batch_size=128, epochs=5, validation_split=0.1)  # OK: MNIST is not sorted
print(model.evaluate(x_te, y_te, verbose=0))
