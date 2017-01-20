from pike import *
import os
import shutil
import json
from subprocess import call, Popen, PIPE
from socket import gethostname
from string import Template

def pikecheck():
    output = Popen(['which', 'pike'], stdout=PIPE, stderr=PIPE).communicate()
    pike = output[0].strip()
    if not pike:
        raise PikeException(output[1])
    return pike

pike = pikecheck()

relpath = '..'
exec(open(relpath + '/etc/make_site.py').read())

@target
def test(*a): pass

@target
def compile(*a): pass

@target
def start(*a): pass

@target
def status(*a): pass
            
@target
def stop(*a): pass

@target
def build(*a): pass