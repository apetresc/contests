import functools
import operator

def largest_product(series: str, span: int) -> int:
    if span > len(series):
        raise ValueError("span must be smaller than string length")
    if span < 0:
        raise ValueError("span must not be negative")
    if not series.isnumeric():
        raise ValueError("digits input must only contain digits")
    
    digits = list(map(int, series))
    return max([functools.reduce(operator.mul, digits[i:i+span])
                for i in range(0, len(digits) - span + 1)])
