import numpy as np
from mlp import MLP, Layer
from sklearn.utils import shuffle
from sklearn import datasets
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
import matplotlib.pyplot as plt

if __name__ == '__main__':
    np.random.seed(123)
    '''
    1. Dataset
    '''
    N = 300
    x, t = datasets.make_moons(N, noise=0.3)
    t = t.reshape(N,1)

    x_train, x_test, t_train, t_test = train_test_split(x, t, test_size=0.2)

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

    epochs = 100
    batch_size = 30
    n_batches = x_train.shape[0] // batch_size

    for epoch in range(epochs):
        train_loss = 0.
        x_, t_ = shuffle(x_train, t_train)

        for n_batch in range (n_batches):
          start = n_batch * batch_size
          end = start + batch_size

          train_loss += train_step(x_[start:end], t_[start:end])

        if epoch % 10 == 0 or epoch == epochs - 1: 
            print ('epoch: {}, loss: {:.3f}'.format(epoch+1, train_loss))

    '''
    4. Decision boundary plotting
    '''
    # Create a grid for decision boundary
    xx, yy = np.meshgrid(
        np.linspace(x[:, 0].min() - 1, x[:, 0].max() + 1, 200),
        np.linspace(x[:, 1].min() - 1, x[:, 1].max() + 1, 200)
    )
    grid = np.c_[xx.ravel(), yy.ravel()]
    preds = model(grid).reshape(xx.shape)

    # Plot the training data points
    plt.figure(figsize=(8, 6))
    plt.scatter(
        x_train[:, 0], x_train[:, 1], c=t_train.ravel(), cmap='viridis', edgecolor='k', label='Train data'
    )
    plt.contourf(
        xx, yy, preds, alpha=0.6, levels=np.linspace(0, 1, 3), cmap='viridis'
    )
    plt.colorbar(label='Model output (probability)')
    plt.title("Decision Boundary with Training Data")
    plt.xlabel("x1")
    plt.ylabel("x2")
    plt.legend()
    plt.show()

    '''
    5. Model evaluation
    '''
    preds = model(x_test) > 0.5
    acc = accuracy_score(t_test, preds)
    print('acc.: {:.3f}'.format(acc))

