import numpy as np
from sklearn.utils import shuffle
import matplotlib.pyplot as plt

class MLP(object):
    '''
    Multi-Perceptron
    '''
    def __init__(self, input_dim, hidden_dim, output_dim):
	# input layer - hidden layer
        self.l1 = Layer(input_dim = input_dim,
			output_dim = hidden_dim,
			activation = sigmoid,
			derivative = dsigmoid) 

	# hidden layer - output layer
        self.l2 = Layer(input_dim = hidden_dim,
			output_dim = output_dim,
			activation = sigmoid,
			derivative = dsigmoid)

        self.layers = [self.l1, self.l2]

    def __call__(self, x):
      return self.forward(x)

    def forward(self, x):
      h = self.l1(x)
      y = self.l2(h)
      return y

def sigmoid(x):
  return 1 / (1 + np.exp(-x))

def dsigmoid(x):
  return sigmoid(x) * (1 - sigmoid(x))

class Layer(object):
    '''
    Layer Coupling
    '''
    def __init__(self, input_dim, output_dim,activation, derivative):
      self.W = np.random.normal(size=(input_dim, output_dim))
      self.b = np.zeros(output_dim)

      self.activation = activation # activation function
      self.derivative = derivative # derivative of activation function

    def __call__(self, x):
      return self.forward(x)

    def forward(self, x):
      self._input = x
      self._pre_activation = np.matmul(x, self.W) + self.b
      return self.activation(self._pre_activation)

    def backward(self, delta, W):
      delta = self.derivative(self._pre_activation)*np.matmul(delta, W.T)
      return delta

    def compute_gradients(self, delta):
      dW = np.matmul(self._input.T, delta)
      db = np.matmul(np.ones(self._input.shape[0]), delta)
      return dW, db

if __name__ == '__main__':
    np.random.seed(123)
    '''
    1. Dataset
    '''
    x = np.array([[0,0],[0,1],[1,0],[1,1]])
    t = np.array([[0],[1],[1],[0]])
    
    '''
    2. Model building
    '''
    model = MLP (2, 2, 1)

    '''
    3. Model learning
    '''
    def compute_loss (t, y):
      return (-t * np.log(y) - (1-t) * np.log(1-y)).sum()

    def train_step(x, t):
      y = model(x)
      for i, layer in enumerate (model.layers[::-1]):
        if i == 0:
          delta = y-t
        else:
          delta = layer.backward(delta, W)

        dW, db = layer.compute_gradients(delta)
        layer.W = layer.W - 0.1 *dW
        layer.b = layer.b - 0.1 *db

        W = layer.W

        loss = compute_loss(t, y) 

        return loss

    epochs = 1000

    for epoch in range(epochs):
        train_loss  = train_step(x, t)

        if epoch % 100 == 0 or epoch == epochs - 1: 
            print ('epoch: {}, loss: {:.3f}'.format(epoch+1, train_loss))

    '''
    4. Model evaluation
    '''
    for input in x:
      print ('{} => {:.3f}'.format(input, model(input)[0]))

    '''
    5. Plotting
    '''

