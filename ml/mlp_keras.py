import numpy as np
from sklearn import datasets
from sklearn.model_selection import train_test_split

import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
from tensorflow.keras import optimizers
from tensorflow.keras.initializers import RandomNormal

import matplotlib.pyplot as plt

if __name__ == '__main__':
    np.random.seed(123)
    tf.random.set_seed(123)

    '''
    1. Dataset
    '''
    N = 300
    x, t = datasets.make_moons(N, noise=0.3)
    t = t.reshape(N,1)

    x_train, x_test, t_train, t_test = train_test_split(x,t, test_size=0.2)

    '''
    2. Model building
    '''
    model = Sequential()
    #model.add(Dense(3, activation='sigmoid'))
    #model.add(Dense(1, activation='sigmoid'))

    model.add(Dense(3,                        
                activation='sigmoid',
                kernel_initializer=RandomNormal(mean=0.0, stddev=1.0, seed=123),
                bias_initializer='zeros'))         
    model.add(Dense(1,
                activation='sigmoid',
                kernel_initializer=RandomNormal(mean=0.0, stddev=1.0, seed=123),
                bias_initializer='zeros'))


    '''
    3. Model learning
    '''
    optimizer = optimizers.SGD(learning_rate=0.1)
    model.compile(optimizer=optimizer, loss='binary_crossentropy', metrics=['accuracy'])
    model.fit(x_train, t_train, epochs=1000, batch_size=10, verbose=1)

    '''
    4. Model evaluation
    '''
    loss, acc = model.evaluate(x_test, t_test, verbose=0)
    print('test_loss: {:.3f}, test_acc: {:.3f}'.format(loss, acc))

    '''
    5. Decision boundary plotting
    '''
    # Create a grid for decision boundary
    xx, yy = np.meshgrid(
        np.linspace(x[:, 0].min() - 1, x[:, 0].max() + 1, 200),
        np.linspace(x[:, 1].min() - 1, x[:, 1].max() + 1, 200)
    )
    grid = np.c_[xx.ravel(), yy.ravel()]
    preds = model(grid).numpy().reshape(xx.shape)

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

