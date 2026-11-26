"""Keras regression of a noisy sine (Exercise 2). Tested in the course hls4ml
environment (Keras 2.11); Keras 3 compatible? check."""
import sys

import keras
from keras import layers

keras.utils.set_random_seed(0)      # repeatable start (Keras 2.11 and Keras 3)

sys.path.insert(0, "../ch23")
from mlp import make_sine           # noqa: E402

x, y = make_sine(n=200, noise=0.2)

model = keras.Sequential([
    keras.Input(shape=(1,)),
    layers.Dense(10, activation="tanh"),
    layers.Dense(1),            # identity output
])
opt = keras.optimizers.SGD(learning_rate=0.05)
model.compile(optimizer=opt, loss="mse")
model.fit(x, y, epochs=500, batch_size=32, verbose=0)
rmse = float(model.evaluate(x, y, verbose=0)) ** 0.5
print(f"training RMSE {rmse:.3f}")     # on the fit points
