from pike import *
import sys
import os
import tarfile
import shutil
import subprocess

skip_srcpkg_check = True
relpath = '..'
exec(open(relpath + '/etc/make_site.py').read())

tools_dir =  os.path.abspath('../tools/')

@target
def default(*a):
    '''default|initialize'''
    for src in os.listdir('.'):
        if not src.endswith('.tar'): continue
        name, version = src[:-4].split('-', 1)
        version_file = os.path.join(tools_dir, name, '.version')
        if not os.path.exists(version_file) \
        or open(version_file).read().strip() < version:
            require(src)

@target
def build(*a):
    global tools_dir
    tools_dir = parameters[0]
    require('default')

@target
def package(match, deps, name, version):
    '''(?P<name>[^-]+)-(?P<version>.+).tar'''
    print('Installing %s in version %s...' % (name, version))
    pkg_dir = os.path.join(tools_dir, name)
    if os.path.exists(pkg_dir):
        shutil.rmtree(pkg_dir)
    tarfile.TarFile(match).extractall()
    extracted_dir = name if os.path.isdir(name) else '%s-%s' % (name, version)
    os.chdir(extracted_dir)
    os.environ['PREFIX'] = tools_dir
    os.environ['PROPATH'] = "."
    subprocess.call([sys.executable, './build.py'])
    if not os.path.exists(pkg_dir):
       print('Installing %s failed' % name)
    else:
       open(pkg_dir + '/.version', 'w').write(version)
    os.chdir('..')
    shutil.rmtree(extracted_dir)
