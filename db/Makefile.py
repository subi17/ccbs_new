from pike import *
import os

rdbmsses = [x for x in os.listdir('.') if os.path.isdir(x)]

@target(['%s/store>initialize' % x for x in rdbmsses])
def initialize(*a): pass

@target(['%s/store>start' % x for x in rdbmsses])
def start(*a): pass

@target(['%s/store>stop' % x for x in rdbmsses])
def stop(*a): pass

@target(['%s/store>migrate' % x for x in rdbmsses])
def migrate(*a): pass

@target(['%s/store>fixtures' % x for x in rdbmsses])
def fixtures(*a): pass
