import numpy as np
import matplotlib.pyplot as plt

# Sigmoid and its derivative
def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def d_sigmoid(x):
    s = sigmoid(x)
    return s * (1 - s)

# Tanh and its derivative
def tanh(x):
    return np.tanh(x)

def d_tanh(x):
    return 1 - np.tanh(x)**2

# Input range
x = np.linspace(-6, 6, 500)

# Compute values
y_sigmoid = sigmoid(x)
dy_sigmoid = d_sigmoid(x)

y_tanh = tanh(x)
dy_tanh = d_tanh(x)

# Plotting
plt.figure(figsize=(12, 5))

# Sigmoid and its derivative
plt.subplot(1, 2, 1)
plt.plot(x, y_sigmoid, label='Sigmoid', color='red')
plt.plot(x, dy_sigmoid, label='d(Sigmoid)', color='blue', linestyle='--')
plt.title('Sigmoid and Its Derivative')
plt.xlabel('x')
plt.ylabel('y')
plt.grid()
plt.legend()

# Tanh and its derivative
plt.subplot(1, 2, 2)
plt.plot(x, y_tanh, label='Tanh', color='green')
plt.plot(x, dy_tanh, label='d(Tanh)', color='purple', linestyle='--')
plt.title('Tanh and Its Derivative')
plt.xlabel('x')
plt.ylabel('y')
plt.grid()
plt.legend()

plt.tight_layout()
plt.show()
