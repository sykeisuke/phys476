"""Keras version of the NumPy MLP on the two-moons data (Exercise 1 of this chapter).

Same data, split and settings as mlp.py of Chapter 23 (noise 0.2, 2-3-1 network,
tanh hidden units, sigmoid output, SGD with eta = 0.5, batch 32, 300 epochs), so the
result can be compared with the NumPy run (validation accuracy 0.983 for H = 3).
The initial weights differ (Keras: Glorot uniform), so the number can differ a little.
Tested in the course hls4ml environment (Keras 2.11); Keras 3 compatible? check.
Run from book/code/ch24:  python3 keras_moons.py
"""
import sys

import keras
from keras import layers

sys.path.insert(0, "../ch23")
from mlp import make_moons          # noqa: E402
from metrics import split           # noqa: E402

keras.utils.set_random_seed(0)      # repeatable initial weights and batches
X, t = make_moons(n=300, noise=0.2)
(X_tr, t_tr), (X_va, t_va), (X_te, t_te) = split(X, t)

model = keras.Sequential([
    keras.Input(shape=(2,)),
    layers.Dense(3, activation="tanh"),     # hidden
    layers.Dense(1, activation="sigmoid"),  # P(t=1|x)
])
model.compile(optimizer=keras.optimizers.SGD(learning_rate=0.5),
              loss="binary_crossentropy", metrics=["accuracy"])
model.fit(X_tr, t_tr, epochs=300, batch_size=32, verbose=0,
          validation_data=(X_va, t_va))
loss, acc = model.evaluate(X_va, t_va, verbose=0)
print(f"validation loss {loss:.3f}, accuracy {acc:.3f}")
