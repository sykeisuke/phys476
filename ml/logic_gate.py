import numpy as np
import pandas as pd

class logic_gate:
    def __init__(self):
        # Initialize weights and threshold
        self.w = np.array([0.0, 0.0])  # Initial weights
        self.theta = 0.0               # Initial threshold
        self.learning_rate = 1.0       # Learning rate

    def step(self, x):
        # Step function: returns 1 if x >= 0, otherwise 0
        return int(x >= 0)

    def predict(self, x):
        # Predict using current weights and threshold
        return self.step(np.dot(self.w, x) - self.theta)

    def update(self, x, t):
        # 1. Calculate the output before the update
        y_before = self.predict(x)

        # 2. Calculate the error
        delta = t - y_before

        # 3. Calculate update values
        dw = self.learning_rate * delta * x
        dtheta = -self.learning_rate * delta

        # 4. Save the current weights and threshold before updating
        current_w = self.w.copy()
        current_theta = self.theta

        # 5. Apply the updates
        self.w += dw
        self.theta += dtheta

        # Return delta, dw, dtheta, and pre-update values
        return delta, dw, dtheta, current_w, current_theta, y_before

# AND gate dataset
data = np.array([
    [0, 0, 0],  # Input (x1, x2) and expected output (t)
    [0, 1, 0],
    [1, 0, 0],
    [1, 1, 1]
])

# Initialize learning process
model = logic_gate()
table = []  # Table to record the learning process

epoch = 0
max_epochs = 100  # Set a maximum number of epochs to avoid infinite loop
while epoch < max_epochs:
    all_correct = True  # Check if all data points are classified correctly
    
    for row in data:
        x = row[:2]  # Input
        t = row[2]   # Expected output
        
        # Perform update (note: now we get y_before as well)
        delta, dw, dtheta, prev_w, prev_theta, y_before = model.update(x, t)
        
        # Record each step in the table
        # Use y_before (output before update) here
        table.append({
            "k": epoch + 1,
            "x1": x[0],
            "x2": x[1],
            "t": t,
            # Record weights and threshold before update
            "w1": prev_w[0],
            "w2": prev_w[1],
            "theta": prev_theta,
            # Record the output before update (y_before)
            "y": y_before,
            "t-y": t - y_before,
            "dw1": dw[0],
            "dw2": dw[1],
            "dtheta": dtheta
        })
        
        # If any error occurs, set all_correct to False
        if delta != 0:
            all_correct = False
    
    epoch += 1
    
    # If all data points are classified correctly, stop learning
    if all_correct:
        break

# Check if the process exceeded the maximum number of epochs
if epoch == max_epochs:
    print("Learning did not converge within the maximum number of epochs.")

# Convert table to a DataFrame for display
df = pd.DataFrame(table)

# Print the table to the console
print(df)

# Optionally, save the table to a CSV file
df.to_csv("and_gate_learning_table.csv", index=False)
print("The learning table has been saved as 'and_gate_learning_table.csv'.")
