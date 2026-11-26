import numpy as np
from sklearn.utils import shuffle
import matplotlib.pyplot as plt

class LogisticRegression(object):
    '''
    Logistic Regression
    '''
    def __init__(self, input_dim, output_dim):
        self.input_dim = input_dim
        self.W = np.random.normal(size=(input_dim,output_dim))  # Initialize weights
        self.b = np.zeros(output_dim)  # Initialize bias

    def __call__(self, x):
        return self.predict(x)

    def predict(self, x):
        return softmax(np.matmul(x, self.W) + self.b)

    def compute_gradients(self, x, t):
        y = self.predict(x)
        delta = y - t
        # Sigma (t_n - y_n) x_n
        dW = np.matmul(x.T, delta) # x.T: transported matrix
        # Sigma (t_n - y_n)
        db = np.matmul(np.ones(x.shape[0]), delta) # shape[0]: # of data points
        return dW, db

def softmax(x):
    return np.exp(x) / np.sum(np.exp(x), axis=1, keepdims=True)

if __name__ == '__main__':
    np.random.seed(123)
    '''
    1. Dataset
    '''
    M = 2 # input data dimension
    K = 3 # class number
    n = 100 # data points in each class
    N = n * K # total data points

    x1 = np.random.randn(n, M) + np.array([0, 10])
    x2 = np.random.randn(n, M) + np.array([5, 5])
    x3 = np.random.randn(n, M) + np.array([10, 0])

    t1 = np.array([[1, 0, 0] for i in range(n)])
    t2 = np.array([[0, 1, 0] for i in range(n)])
    t3 = np.array([[0, 0, 1] for i in range(n)])

    x = np.concatenate((x1, x2, x3), axis=0)
    t = np.concatenate((t1, t2, t3), axis=0)

    '''
    2. Model building
    '''
    model = LogisticRegression(input_dim=M, output_dim=K)

    '''
    3. Model learning
    '''
    def compute_loss (t, y):
        return (-t * np.log(y)).sum(axis=1).mean()

    def train_step(x, t):
        dW, db = model.compute_gradients(x, t)
        model.W = model.W - 0.1 *dW
        model.b = model.b - 0.1 *db
        loss = compute_loss(t, model(x)) # model(x) is called via __call__ method
        return loss

    epochs = 10
    batch_size = 50
    n_batches = x.shape[0] // batch_size

    for epoch in range(epochs):
        train_loss = 0.
        x_, t_ = shuffle(x, t)

        for n_batch in range(n_batches):
            start = n_batch * batch_size
            end = start + batch_size

            train_loss += train_step(x_[start:end], t_[start:end])

        if epoch % 10 == 0 or epoch == epochs - 1: 
            print ('epoch: {}, loss: {:.3f}'.format(epoch+1, train_loss))

    '''
    4. Model evaluation
    '''
    x_, t_ = shuffle(x, t)
    preds = model(x_[0:5])
    classified = \
        np.argmax(t_[0:5], axis=1) == np.argmax(preds[0:5], axis=1)
    print('Prediction matched:', classified)

    '''
    5. Plotting
    '''
    def plot_decision_boundary_lines(model, x):
        x_min, x_max = x[:, 0].min() - 1, x[:, 0].max() + 1
        x_vals = np.linspace(x_min, x_max, 100)

        for i in range(model.W.shape[1] - 1):
            w_diff = model.W[:, i] - model.W[:, i + 1]
            b_diff = model.b[i] - model.b[i + 1]
            y_vals = -(w_diff[0] * x_vals + b_diff) / w_diff[1]
            plt.plot(x_vals, y_vals, label=f"Boundary {i+1}-{i+2}")

    plt.figure(figsize=(8, 6))
    for i, label in enumerate(['Class 1', 'Class 2', 'Class 3']):
        plt.scatter(x[t[:, i] == 1, 0], x[t[:, i] == 1, 1], label=f'{label}', alpha=0.6, edgecolor='k')

    plot_decision_boundary_lines(model, x)

    plt.title('Data Distribution After Classification with Decision Boundaries')
    plt.xlabel('x1')
    plt.ylabel('x2')
    plt.legend()
    plt.show()

