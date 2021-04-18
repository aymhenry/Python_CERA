# from .decorators import *

# @show_input_output("ALL")


def interpolation(x_value, y_value, x_series, y_series, data):

    def find_nerest_index(flt_value, lst):
        items_x = [itm for itm in lst if flt_value >= itm]
        print(items_x)
        return len(items_x)

    def interplate(x_val, x1, x2, y1, y2):
        if x1 == x2:
            return y2
        else:
            return y1 + (x_val - x1) * (y2 - y1) / (x2 - x1)

    if len(x_series) != len(data[0]) or len(y_series) != len(data):
        raise ValueError("Reading value out of range, 1")

    if x_value > max(x_series) or y_value > max(y_series)\
            or x_value < min(x_series) or y_value < min(y_series):
        raise ValueError("Reading value out of range, 2")

    x_pos = find_nerest_index(x_value, x_series) - 1
    y_pos = find_nerest_index(y_value, y_series) - 1

    if x_pos + 1 == len(x_series):
        x_pos_next = x_pos
    else:
        x_pos_next = x_pos + 1

    if y_pos + 1 == len(y_series):
        y_pos_next = y_pos
    else:
        y_pos_next = y_pos + 1

    value1 = interplate(x_val=x_value,
                        x1=x_series[x_pos],
                        x2=x_series[x_pos_next],
                        y1=data[y_pos][x_pos],
                        y2=data[y_pos][x_pos_next]
                        )

    value2 = interplate(x_val=x_value,
                        x1=x_series[x_pos],
                        x2=x_series[x_pos_next],
                        y1=data[y_pos_next][x_pos],
                        y2=data[y_pos_next][x_pos_next]
                        )

    value = interplate(x_val=y_value,
                       x1=y_series[y_pos],
                       x2=y_series[y_pos_next],
                       y1=value1,
                       y2=value2
                       )
    return value
