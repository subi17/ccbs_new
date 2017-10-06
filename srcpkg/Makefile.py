from pike import *
import sys
import os
import tarfile
import shutil
import subprocess
from contextlib import closing

skip_srcpkg_check = True
relpath = '..'
exec(open(relpath + '/etc/make_site.py').read())

tools_dir =  os.path.abspath('../tools/')

depdict = {}
error = []

def versiontuple(v):
    filled = []
    for point in v.split("."):
        filled.append(point.zfill(8))
    return tuple(filled)

def getpackages():
    # Get tuple list of packages (package, version)
    # Store only the highest package version
    packages = []
    for src in os.listdir('.'):
        if not src.endswith('.tar'): continue
        name, version = src[:-4].split('-', 1)
        ver = next((p for p, v in packages if p == name), None)
        if not (ver and versiontuple(ver) > versiontuple(version)):
            packages.append((name, version))
    return packages

def deprecursion(basepackage, package, packagelist):
    if not package in depdict:
        return

    for deppackage in depdict[package]:
        if deppackage == basepackage:
            error.append('package "{0}" depends package "{1}" and vice versa'.format(deppackage, package))

        elif not deppackage in packagelist:
            packagelist.append(deppackage)
            deprecursion(basepackage, deppackage, packagelist)
    return


@target
def default(*a):
    '''default|initialize'''

    packages = getpackages()

    depcount = {}
    for package, version in packages:
        version_file = os.path.join(tools_dir, package, '.version')
        if not os.path.exists(version_file) \
        or open(version_file).read().strip() < version:
            depcount[package] = 1
            deplist = []

            try:
                with tarfile.open('{0}-{1}.tar'.format(package,version)) as tf:
                    dependencyfile = next((x for x in tf.getnames() if x in ['{0}/.dependencies'.format(package), '{0}-{1}/.dependencies'.format(package, version)]))
                    with closing(tf.extractfile(dependencyfile)) as f:
                        for line in f:
                            if not line.startswith('#'):
                                try:
                                    deplist.append(tuple(line.strip(",\n").split("-")))
                                except:
                                    error.append('"{0}": Unable to parse .dependencies line "{1}"'.format(package, line))
                                    continue
            except (KeyError, StopIteration):
                pass

            for deppackage, depversion in deplist:
                if not deppackage in [x[0] for x in packages]:
                    error.append('"{0}": Depends package {1} which is not available'.format(package, deppackage))
                    continue
                availver = next(x for x in packages if x[0] == deppackage)[1]
                if deppackage == package:
                    error.append('"{0}": depends itself'.format(package))
                elif versiontuple(depversion) > versiontuple(availver):
                    error.append('"{0}": version "{1}" is needed for "{2}" but only "{3}" is available'.format(package, depversion, deppackage, availver))
                else:
                    if not package in depdict:
                        depdict[package] = [deppackage]
                    elif deppackage in depdict[package]:
                        error.append('package "{0}" depends package "{1}" multiple times'.format(package, deppackage))
                    else:
                        depdict[package].append(deppackage)

    if not error:
        for package in depcount:
            crosserror = []
            packagelist = []
            deprecursion(package, package, packagelist)
            if crosserror:
                error.append(crosserror)
            else:
                for pkg in packagelist:
                    if pkg in depcount:
                        depcount[pkg] += 1

    if error:
        for item in error:
            print(item)
        raise PikeException('There were errors in packages')

    installorder = sorted(depcount, key=depcount.get, reverse = True)

    for package in installorder:
        availver = next(x for x in packages if x[0] == package)[1]
        require('{0}-{1}.tar'.format(package,availver))

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

    if os.path.isfile('{0}/progress.cfg.edit'.format(dlc)):
        os.environ['PROCFG'] = '{0}/progress.cfg.edit'.format(dlc)

    subprocess.call([sys.executable, './build.py'])
    if not os.path.exists(pkg_dir):
       print('Installing %s failed' % name)
    else:
       open(pkg_dir + '/.version', 'w').write(version)
    os.chdir('..')
    shutil.rmtree(extracted_dir)
