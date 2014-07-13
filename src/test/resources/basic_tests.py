#  MOIS: Python Test
#  Copyright (C) 2014 University of Edinburgh School of Informatics
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
