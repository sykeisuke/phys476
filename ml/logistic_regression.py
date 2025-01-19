import numpy as np
import matplotlib.pyplot as plt

class LogisticRegression(object):
    '''
    Logistic Regression
    '''
    def __init__(self, input_dim):
        self.input_dim = input_dim
        self.w = np.random.normal(size=(input_dim,))  # Initialize weights
        self.b = 0.0  # Initialize bias

    def __call__(self, x):
        return self.predict(x)

    def predict(self, x):
        return sigmoid(np.matmul(x, self.w) + self.b)

    def compute_gradients(self, x, t):
        y = self.predict(x)
        delta = y - t
        # Sigma (t_n - y_n) x_n
        dw = np.matmul(x.T, delta) # x.T: transported matrix
        # Sigma (t_n - y_n)
        db = np.matmul(np.ones(x.shape[0]), delta) # shape[0]: # of data points
        return dw, db

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

if __name__ == '__main__':
    np.random.seed(123)
    '''
    1. Dataset (OR)
    '''
    x = np.array([[0,0], [0,1], [1,0], [1,1]])
    t = np.array([0, 1, 1, 1])

    '''
    2. Model building
    '''
    model = LogisticRegression(input_dim=2)

    '''
    3. Model learning
    '''
    def compute_loss (t, y):
        return (-t * np.log(y) - (1-t) * np.log(1-y)).sum()

    def train_step(x, t):
        dw, db = model.compute_gradients(x, t)
        model.w = model.w - 0.1 *dw
        model.b = model.b - 0.1 *db
        loss = compute_loss(t, model(x)) # model(x) is called via __call__ method
        return loss

    epochs = 100
    loss_history = []
    output_history = []

    for epoch in range(epochs):
        train_loss = train_step(x,t)
        loss_history.append(train_loss)
        output_history.append([model(input) for input in x])

        if epoch % 10 == 0 or epoch == epochs - 1: 
            print ('epoch: {}, loss: {:.3f}'.format(epoch+1, train_loss))

    '''
    4. Model evaluation
    '''
    for input in x:
        print('{} => {:.3f}'.format(input, model(input))) # model input and output values

    '''
    5. Plotting
    '''
    # Plot loss vs epoch
    plt.figure(figsize=(10, 6))
    plt.plot(range(1, epochs + 1), loss_history, label='Loss')
    plt.xlabel('Epoch')
    plt.ylabel('Loss')
    plt.title('Loss vs Epoch')
    plt.grid(True)
    plt.legend()
    plt.show()

    # Plot model output convergence for [0,0], [0,1], [1,0], [1,1]
    outputs_per_epoch = np.array(output_history)
    plt.figure(figsize=(10, 6))
    for i, point in enumerate(x):
        plt.plot(range(1, epochs + 1), outputs_per_epoch[:, i], label=f"Input {point}")

    plt.xlabel('Epoch')
    plt.ylabel('Model Output')
    plt.title('Model Output Convergence for OR Gate')
    plt.grid(True)
    plt.legend()
    plt.show()

