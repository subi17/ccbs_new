###############################################################################
# Main Makefile for a ccbs production instance                                #
# This file needs not be changed. All configuration ist in the files          #
# etc/site.make and etc/config.make                                           #
###############################################################################

from pike import *
import sys, os
import shutil
from subprocess import call

if sys.version_info[:3] < (2,6,0):
    raise PikeException('Python version too old. Require at least 2.6')

skip_srcpkg_check = True
relpath = '.'
execfile(relpath + '/etc/make_site.py')


@file_target(['relink_var', 'srcpkg>initialize', 'db>initialize'])
def initialized(*a):
    '''.initialized'''
    open(a[0], 'w').close()
    print('Initialization successful')

@target(['.initialized'])
def initialize(*a):
    '''default|initialize|init'''
    pass


@target(['%s>rundaemons' % x for x in modules])
def rundaemons(*a): pass

@target(['%s>stopdaemons' % x for x in modules])
def stopdaemons(*a): pass

@target
def relink_var(*a):
    if not os.path.exists('../var'):
        print('Creating var directory in %s.' % os.path.abspath('..'),
              'Please check permissions!')
    for subdir in ['', '/log', '/log/eventlog', '/log/usagelog',
                               '/log/errorlog',
                       '/run', '/tmp']:
        if not os.path.exists('../var' + subdir):
            os.mkdir('../var' + subdir)
    if not os.path.islink('var'):
        if os.path.isdir('var'):
            shutil.rmtree('var')
        os.symlink('../var', 'var')

@target('.activated')
def activate(*a): pass

@file_target
def activated(*a):
    '''.activated'''
    symlink = '../' + appname
    if os.path.exists(symlink):
        if os.path.exists(symlink + '/.activated'):
            call(['pike', '-C', symlink, 'deactivate'])
        if os.path.islink(symlink):
            os.unlink(symlink)
        else:
            os.rename(symlink, symlink + '_old')

    require('db>migrate')
    require('rundaemons')
    os.symlink(os.path.basename(os.getcwd()), symlink)
    open('.activated', 'w').close()

@target
def deactivate(*a):
    os.unlink('.activated')
