"""Keras MLP for the toy Monte Carlo sample (Chapter 24).

Tested in the course hls4ml environment (Keras 2.11, set up in Week 12, see Chapter 24);
Keras 3 compatible? check.  Data loading and the split are in load_toymc.py.
Run:  python3 keras_toymc.py path/to/toymc_0.npy
"""
import sys

import keras
from keras import layers

keras.utils.set_random_seed(0)      # repeatable start (Keras 2.11 and Keras 3)

from load_toymc import X_tr, t_tr, X_va, t_va, X_te, t_te

sys.path.insert(0, "../ch23")
from metrics import roc, auc    # noqa: E402

scale = 64.0          # inputs 0..50 -> 0..0.78 (power of 2: free on an FPGA)

model = keras.Sequential([
    keras.Input(shape=(3,)),
    layers.Dense(16, activation="relu"),
    layers.Dense(16, activation="relu"),
    layers.Dense(1, activation="sigmoid"),
])
model.compile(optimizer=keras.optimizers.Adam(learning_rate=1e-3),
              loss="binary_crossentropy", metrics=["accuracy"])
model.fit(X_tr / scale, t_tr, batch_size=256, epochs=20,
          validation_data=(X_va / scale, t_va))
y_va = model.predict(X_va / scale).ravel()
print("validation AUC", auc(*roc(t_va, y_va)))
# only after all choices are made:
# print(model.evaluate(X_te / scale, t_te))
