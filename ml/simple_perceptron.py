import numpy as np
import matplotlib.pyplot as plt

class SimplePerceptron(object):
    '''
    Simple Perceptron Class  
    '''
    def __init__(self, input_dim):
        self.input_dim = input_dim
        self.w = np.random.normal(size=(input_dim,))  # Initialize weights
        self.theta = 0.0  # Initialize threshold (bias)

    def predict(self, x):
        # Predict using current weights and threshold
        return self.step(np.dot(self.w, x) - self.theta)

    def compute_deltas(self, x, t):
        # Compute deltas for weight and threshold updates
        y = self.predict(x)
        delta = t - y  # Error (expected - predicted)
        dw = delta * x  # Weight updates
        dtheta = -delta  # Threshold update
        return dw, dtheta

    @staticmethod
    def step(x):
        # Step function: returns 1 if x > 0, otherwise 0
        return (x > 0).astype(int)

if __name__ == '__main__':

    '''
    1. Dataset
    '''
    np.random.seed(123)
    d = 2
    N = 20

    mean = 5

    # Generate two classes of data
    data1 = np.random.randn(N // 2, d) + np.array([0, 0])
    data2 = np.random.randn(N // 2, d) + np.array([mean, mean])

    label1 = np.zeros(N // 2)  # Labels for class 0
    label2 = np.ones(N // 2)   # Labels for class 1

    x = np.concatenate((data1, data2), axis=0)
    t = np.concatenate((label1, label2))

    '''
    2. Model building
    '''
    model = SimplePerceptron(input_dim=d)

    '''
    3. Model learning
    '''
    def compute_loss(dw, dtheta):
        # Check if all updates are zero (indicating convergence)
        return np.all(dw == 0) and (dtheta == 0)

    def train_step(x, t):
        # Perform one training step
        dw, dtheta = model.compute_deltas(x, t)
        loss = compute_loss(dw, dtheta)
        model.w += dw
        model.theta += dtheta
        return loss

    def plot_decision_boundary(model, x, t, epoch):
        # Plot the decision boundary
        plt.figure(figsize=(8, 6))
        plt.scatter(x[:N // 2, 0], x[:N // 2, 1], label="Class 0", color="blue")
        plt.scatter(x[N // 2:, 0], x[N // 2:, 1], label="Class 1", color="red")

        x_vals = np.linspace(np.min(x[:, 0]) - 1, np.max(x[:, 0]) + 1, 100)
        if model.w[1] != 0:  # Avoid division by zero
            y_vals = -(model.w[0] * x_vals - model.theta) / model.w[1]
            plt.plot(x_vals, y_vals, color="green", label="Decision Boundary")

        plt.title(f"Epoch {epoch}")
        plt.xlabel("x1")
        plt.ylabel("x2")
        plt.legend()
        plt.grid()
        plt.show()

    epoch = 0

    while True:
        classified = True
        for i in range(N):
            loss = train_step(x[i], t[i])
            classified = classified and loss
        epoch += 1
        plot_decision_boundary(model, x, t, epoch)
        if classified:
            break

    '''
    4. Model evaluation
    '''
    print('w:', model.w)
    print('theta:', model.theta)
