import numpy as np

def my_polyfit (x_array, y_array, deg):
    x_np = np.array(x_array)
    y_np = np.array(y_array)
    res = list(np.polyfit(x_np, y_np, deg))
    lst = list(map(float, res))
    lst = [round(x, 4) for x in lst]
    return lst
