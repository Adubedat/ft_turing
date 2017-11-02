import numpy as np

def my_polyfit (x_array, y_array, deg):
    x_np = np.array(x_array)
    y_np = np.array(y_array)
    np.polyfit(x_np, y_np, deg)
