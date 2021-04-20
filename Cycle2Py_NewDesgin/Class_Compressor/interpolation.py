from decorators import *
from ErrorException import ErrorException

@show_input_output("ALL")
# interpolation job
def interpolation(x_value, y_value, x_series, y_series, data):

    def find_nerest_index(flt_value, lst):
        items_x = [itm for itm in lst if flt_value >= itm]
        return len(items_x)

    def interplate(x_val, x1, x2, y1, y2):
        if x1 == x2:
            return y2
        else:
            return y1 + (x_val - x1) * (y2 - y1) / (x2 - x1)

    # set init values
    x1_out = x2_out = y1_out = y2_out = -1
    
    if len(x_series) != len(data[0]) or len(y_series) != len(data):
        raise ErrorException('Reading value out of range', 'Comp1001')
    
    # if x_value > max(x_series) or y_value > max(y_series) \
    #        or x_value < min(x_series) or y_value < min(y_series):
    #    print("\n\n Temp x_series", x_series, " x_value=", x_value)
    #    print("\n Pressure y_series", y_series, "y_value=", y_value)
    #    # raise ErrorException('Reading value above or over range', 'Comp1002')
         
    x_pos = find_nerest_index(x_value, x_series) - 1
    y_pos = find_nerest_index(y_value, y_series) - 1
    
    # x position
    if x_pos == -1:     # x_value less than min
        x_pos = 0
        x_pos_next = 1
        
    elif x_pos + 1 >= len(x_series):    # x_value more than max or max
        x_len = len(x_series)
        x_pos = x_len - 2
        x_pos_next = x_len - 1
    
    else:       # x_value in between
        x_pos_next = x_pos + 1

    # y position
    if y_pos == -1:     # y_value less than min
        y_pos = 0
        y_pos_next = 1
        
    if y_pos + 1 >= len(y_series):   # y_value more than max or max
        y_len = len(y_series)
        y_pos = y_len - 2
        y_pos_next = y_len - 1
        
    else:   # y_value in between
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
