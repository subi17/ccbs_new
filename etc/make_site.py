###############################################################################
# Mandatory initialization
# All ccbs-pikefiles define their relative position to the work_dir as
# relpath and then execute this file.
# It writes the etc/site.py file, if it does not exist and executes it.
# etc/site.py in turn executes etc/config.py, so that this file is the
# only configuration file that needs calling.
###############################################################################
import os
import sys
from glob import glob

assert sys.version_info[:3] >= (2,6,0), 'Python version too old. Need >= 2.6'

def versiontuple(v):
    filled = []
    for point in v.split("."):
        filled.append(point.zfill(8))
    return tuple(filled)

def which(program):
    import os
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file

    return None

if not os.path.exists(relpath + '/etc/site.py'):
    fake_site = {'dlc': 'foo', 'work_dir': 'bar'}
    exec(open(relpath + '/etc/config.py').read(), globals(), fake_site)
    proversion = fake_site['proversion']

    newestversion="0"
    versionfiles = glob('/opt/progress/*/version')

    for versionfile in versionfiles:
        with open(versionfile, 'r') as f:
            found_version = f.readline().split(' ')[2]
        if versiontuple(found_version) >= versiontuple(newestversion):
            newestversion, dlc = (found_version, versionfile[:-8])

    assert versiontuple(newestversion) >= versiontuple(proversion), \
         'Cannot find Progress version >= ' + proversion

    print('Using the newest installed Progress version %s in directory %s' % \
             (newestversion, dlc))

    if os.path.exists(relpath + '/.safeproduction'):
        environment = 'safeproduction'
    elif os.path.exists(relpath + '/.DeployMakefile.py'):
        environment = 'development'
    else:
        environment = 'production'

    lighttpd_location = which('lighttpd')

    # Assume location of the lighttpd if not in path
    if lighttpd_location == None:
        lighttpd_location = '/usr/lighttpd/1.4/sbin/lighttpd'

    fd = open(relpath + '/etc/site.py', 'w')
    fd.write('# Host specific configuration (not under RC)\n')
    fd.write('from os import environ as ENV\n')
    fd.write("%-14s = '%s'\n" % ('dlc', dlc))
    fd.write("%-14s = '%s'\n" % ('work_dir', os.path.abspath(relpath)))
    fd.write("%-14s = '%s'\n" % ('environment', environment))
    fd.write("%-14s = '%s'\n" % ('lighttpd_location', lighttpd_location))
    fd.write("ENV%-11s = '%s'\n" % ("['TERM']", 'xterm'))
    fd.write("ENV%-11s = '%s%s'\n" % ("['DBUTIL']", dlc, '/bin/_dbutil'))
    if environment == 'development' and os.environ.get('PROCFG'):
        fd.write("ENV%-11s = '%s'\n" % ("['PROCFG']", os.environ['PROCFG']))
    fd.write("\nexec(open(work_dir + '/etc/config.py').read())\n")
    fd.close()

exec(open(relpath + '/etc/site.py').read())
os.environ['DLC'] = dlc
os.environ['display_banner'] = 'no'
os.environ['PROTERMCAP'] = work_dir + '/etc/protermcap'

def modgen():
    for mod in modules:
        if environment != 'development':
            yield '{0}/{0}.pl'.format(mod)
        yield mod
    yield 'tms_support'
    yield 'tools'
    yield 'tools/stompAdapter'

if environment == 'safeproduction':
    os.environ['PROPATH'] = '/tmsapps,'
else:
    os.environ['PROPATH'] = ''

os.environ['PROPATH'] += ','.join(['%s/%s' % (work_dir, x) \
                          for x in modgen()]) + ',.'

if environment == 'development':
    databases.extend(cdr_databases)
    del cdr_databases[:]

sys.path.insert(0, work_dir + '/tools')

if not 'skip_srcpkg_check' in locals() and os.path.exists(relpath + '/srcpkg'):
    srcpkg_need_update = []
    for src in os.listdir(relpath + '/srcpkg'):
        if not src.endswith('.tar'): continue
        name, version = src[:-4].split('-', 1)
        version_file = relpath + '/tools/%s/.version' % name
        if not os.path.exists(version_file) \
        or open(version_file).read().strip() < version:
            srcpkg_need_update.append(name)
    if srcpkg_need_update:
        print('WARNING: Tools %s need updating. Run `pike -C srcpkg`!' % \
                          ', '.join(srcpkg_need_update))

