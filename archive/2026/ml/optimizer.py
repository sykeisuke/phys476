from tensorflow.keras import optimizers

# Momentum Method
optimizer = optimizers.SGD(learning_rate=0.01, momentum=0.9)
model.compile(optimizer = optimizer, 
	      loss = 'sparse_categorical_crossentropy', 
	      metrics = ['accuracy'])

# AdaGrad
optimizer = optimizers.Adagrad(learning_rate=0.01)

# RMSprop (rho is equivalent to beta)
optimizer = optimizers.RMSprop(learning_rate=0.001, rho=0.9) 

# Adam 
optimizer = optimizers.Adam(learning_rate=0.001, beta_1=0.9, beta_2=0.999)

