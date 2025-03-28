import numpy as np
import matplotlib.pyplot as plt

# Step function
def step(x):
    return np.where(x >= 0, 1, 0)

# Sigmoid function
def sigmoid(x):
    return 1 / (1 + np.exp(-x))

# Tanh function
def tanh(x):
    return np.tanh(x)

# ReLU function
def relu(x):
    return np.maximum(0, x)

# Leaky ReLU function
def leaky_relu(x, alpha=0.01):
    return np.where(x > 0, x, alpha * x)

# Input range
x = np.linspace(-10, 10, 1000)

# Compute function outputs
y_step = step(x)
y_sigmoid = sigmoid(x)
y_tanh = tanh(x)
y_relu = relu(x)
y_leaky_relu = leaky_relu(x)

# Plotting
plt.figure(figsize=(12, 8))

# Step
plt.subplot(2, 3, 1)
plt.plot(x, y_step, label="Step", color="blue")
plt.title("Step Function")
plt.grid()
plt.legend()

# Sigmoid
plt.subplot(2, 3, 2)
plt.plot(x, y_sigmoid, label="Sigmoid", color="red")
plt.title("Sigmoid Function")
plt.grid()
plt.legend()

# Tanh
plt.subplot(2, 3, 3)
plt.plot(x, y_tanh, label="Tanh", color="green")
plt.title("Tanh Function")
plt.grid()
plt.legend()

# ReLU
plt.subplot(2, 3, 4)
plt.plot(x, y_relu, label="ReLU", color="purple")
plt.title("ReLU Function")
plt.grid()
plt.legend()

# Leaky ReLU
plt.subplot(2, 3, 5)
plt.plot(x, y_leaky_relu, label="Leaky ReLU", color="orange")
plt.title("Leaky ReLU Function")
plt.grid()
plt.legend()

plt.tight_layout()
plt.show()
