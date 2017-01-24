###############################################################################
# Main Makefile for a ccbs instance                                           #
# This file needs not be changed. All configuration ist in the files          #
# etc/site.make and etc/config.make                                           #
###############################################################################

import subprocess

def get_distribution_name():
    proc1 = subprocess.Popen(['git', 'branch'], stdout=subprocess.PIPE)
    proc2 = subprocess.Popen(['awk', '/^\\*/{print $2}'], stdin=proc1.stdout, stdout=subprocess.PIPE)
    branch = proc2.communicate()[0].strip().decode('utf-8')
    branch = branch.replace(' ', '_').replace(':', '_')
    if branch != 'default':
        return '%s-%s-%s' % (appname, appversion, branch)
    else:
        return '%s-%s' % (appname, appversion)


from pike import *
import sys
import os
import tempfile
import shutil
from socket import getservbyname

assert os.path.exists('.usepike'), 'pike is disabled'

def versiontuple(v):
    filled = []
    for point in v.split("."):
        filled.append(point.zfill(8))
    return tuple(filled)

def customize():
    print('This instance is not yet customized. Let\'s do it now!')
    print('Name of the instance: ',)
    appname = sys.stdin.readline().strip()
    appversion = '0.1'
    print('Required progress version: ',)
    proversion = sys.stdin.readline().strip().lower()
    assert versiontuple(proversion) > versiontuple('10.1b'), 'Progress version too old. Need > 10.1b'
    # TODO
    fd = open('etc/config.py', 'w')
    fd.write('# Project configuration (should be under RC)\n')
    fd.write('from os import environ as ENV\n\n')
    fd.write('%-14s = [%s]\n' % ('mpro', "'%s/bin/mpro' % dlc,\n                  '-pf', '%s/etc/pf/formats.pf' % (work_dir)"))
    fd.write("%-14s = '%s'\n" % ('appname', appname))
    fd.write("%-14s = '%s'\n" % ('appversion', appversion))
    fd.write("%-14s = '%s'\n" % ('proversion', proversion))
    fd.write("ENV%-11s = %s\n" % ("['DLC']", 'dlc'))
    fd.write("ENV%s = '%s'\n" % ("['display_banner']", 'no'))
    fd.close()

if not os.path.exists('etc/config.py'):
    customize()
skip_srcpkg_check = True
relpath = '.'
exec(open(relpath + '/etc/make_site.py', 'rb').read())


@file_target(['srcpkg>initialize', 'db>initialize'])
def initialized(*a):
    '''.initialized'''
    open(a[0], 'w').close()
    print('Initialization successful')

@target(['.initialized'])
def initialize(*a):
    '''default|initialize|init'''
    pass


@target(['.initialized', 'db>start'] + \
        ['%s>compile' % x for x in modules] + \
        ['%s>test' % x for x in modules])
def test(*a): pass

@target(['%s>rundaemons' % x for x in modules])
def rundaemons(*a): pass

@target(['%s>stopdaemons' % x for x in modules])
def stopdaemons(*a): pass


def all_migrations_executed():
    for migration in os.listdir('db/progress/migrations'):
        if not migration.endswith('.py'): continue
        if not os.path.exists('db/progress/store/migrations/' + migration):
            return False
    return True

@target
def dist(*a):
    # Commented for TDC: There is no staging server, so dist is made on prod.
    # assert all_migrations_executed()
    rdbmesses = [x for x in os.listdir('db') if os.path.isdir('db/' + x)]
    build_dir = tempfile.mkdtemp()
    dist_basename = get_distribution_name()
    dist_dir = os.path.join(build_dir, dist_basename)
    os.mkdir(dist_dir)
    print('Building into %s...' % build_dir)
    for directory in ['tools', 'db'] + \
                     ['db/%s/store' % x for x in rdbmesses]:
        for dir in (os.path.join(dist_dir, *directory.split('/')[:ii + 1]) \
                            for ii in range(directory.count('/') + 1)):
            if not os.path.exists(dir):
                os.mkdir(dir)
        if os.path.exists(directory + '/Makefile.py'):
            shutil.copyfile(directory + '/Makefile.py',
                            os.path.join(dist_dir, directory) + '/Makefile.py')
    for directory in ['etc', 'srcpkg'] + \
                     ['db/%s/migrations' % x for x in rdbmesses]:
        shutil.copytree(directory, os.path.join(dist_dir, directory))
    os.unlink(dist_dir + '/etc/site.py')
    for mod in modules:
        require('%s>build' % mod, [os.path.join(dist_dir, mod)])
    shutil.copyfile('.DeployMakefile.py', dist_dir + '/Makefile.py')
    print('Archiving...')
    subprocess.call(['tar', '-cf', dist_basename + '.tar',
                            '-C', build_dir, dist_basename])
    shutil.rmtree(build_dir)
    if parameters and parameters[0] in ['bz2', 'gz']:
        print('Compressing...')
        if parameters[0] == 'gz':
            subprocess.call('gzip', dist_basename + '.tar')
        else:
            subprocess.call('bzip2', dist_basename + '.tar')

