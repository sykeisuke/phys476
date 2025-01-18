import numpy as np
import matplotlib.pyplot as plt

# Step function
def step(x):
    """
    Step function: Returns 1 if x >= 0, otherwise 0.
    """
    return np.where(x >= 0, 1, 0)

# Sigmoid function
def sigmoid(x):
    """
    Sigmoid function: Returns a value between 0 and 1 for any input x.
    Formula: 1 / (1 + exp(-x))
    """
    return 1 / (1 + np.exp(-x))

# Visualization of step and sigmoid functions
x = np.linspace(-10, 10, 100)  # Generate 100 points between -10 and 10
y_step = step(x)               # Step function output
y_sigmoid = sigmoid(x)         # Sigmoid function output

# Plotting the functions
plt.figure(figsize=(10, 5))

# Step function plot
plt.subplot(1, 2, 1)
plt.plot(x, y_step, label="Step Function", color="blue")
plt.title("Step Function")
plt.xlabel("x")
plt.ylabel("y")
plt.grid()
plt.legend()

# Sigmoid function plot
plt.subplot(1, 2, 2)
plt.plot(x, y_sigmoid, label="Sigmoid Function", color="red")
plt.title("Sigmoid Function")
plt.xlabel("x")
plt.ylabel("y")
plt.grid()
plt.legend()

plt.tight_layout()
plt.show()
