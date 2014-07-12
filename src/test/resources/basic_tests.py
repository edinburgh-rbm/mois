## Functions to support the unit tests

def incx(t, tau, x, y):
    "Increment x"
    x += 1
    return x

def tooMany(t, tau):
    "Return more values than we were asked for"
    return 1,2,3,4,5

def tooFew(t, tau):
    "Return fewer values than we were asked for"
    return 1,2

def none(t, tau):
    "Return no values"
    
def one(t, tau):
    "Return one value"
    return 1

def error():
    "Don't take the right arguments"

def time(t, tau):
    "Return the time parameters"
    return t, tau
