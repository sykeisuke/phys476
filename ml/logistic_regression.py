import numpy as np

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

for epoch in range(epochs):
    train_loss = train_step(x,t)

    if epoch % 10 == 0 or epoch == epochs - 1: 
        print ('epoch: {}, loss: {:.3f}'.format(
            epoch+1,
            train_loss # loss value
        ))

    '''
    4. Model evaluation
    '''
    for input in x:
        print('{} => {:.3f}'.format(input, model(input))) # model input and output values


