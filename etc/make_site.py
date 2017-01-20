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
import socket

assert os.path.exists(relpath + '/.usepike'), 'pike is disabled'

assert sys.version_info[:3] >= (2,6,0), 'Python version too old. Need >= 2.6'

if not os.path.exists(relpath + '/etc/site.py'):
    fake_site = {'dlc': 'foo', 'work_dir': 'bar'}
    exec(open(relpath + '/etc/config.py').read(), globals(), fake_site)
    proversion = fake_site['proversion']
    for dlc in [os.environ.get('DLC', '/non-exist')] + \
               ['/opt/%s%s' % (x, y) \
                    for x in ['progress', 'dlc'] \
                    for y in ['/' + proversion, '/' + proversion[:4], '']]:
        if os.path.exists(dlc + '/version'):
            found_version = open(dlc + '/version').readline().split(' ')[2]
            found_version = found_version.lower().replace('.', '')
            if found_version.startswith(proversion):
                break
            else:
                print('Found not matching version %s in %s' % \
                        (found_version, dlc))
    else:
        raise PikeException('No progress %s installation found' % proversion)

    a_database = '%s_%s' % (fake_site['appname'], fake_site['databases'][0])
    service_suffix = ''
    while True:
        try:
            socket.getservbyname(a_database + service_suffix, 'tcp')
            break
        except:
            print('Ports for databases not found in /etc/services.')
            print('Add them or use unix sockets or provide a suffix: ',)
            service_suffix = sys.stdin.readline().strip()
        if not service_suffix: break

    environment = 'development'
#    if os.path.exists(relpath + '/.DeployMakefile.py'):
#        environment = 'development'
#    else:
#        environment = 'production'

    fd = open(relpath + '/etc/site.py', 'w')
    fd.write('# Host specific configuration (not under RC)\n')
    fd.write('from os import environ as ENV\n')
    fd.write("%-14s = '%s'\n" % ('dlc', dlc))
    fd.write("%-14s = '%s'\n" % ('work_dir', os.path.abspath(relpath)))
    fd.write("%-14s = '%s'\n" % ('environment', environment))
    fd.write("%-14s = '%s'\n" % ('service_suffix', service_suffix))
    fd.write("ENV%-11s = '%s'\n" % ("['TERM']", 'xterm'))
    if environment == 'development' and os.environ.get('PROCFG'):
        fd.write("ENV%-11s = '%s'\n" % ("['PROCFG']", os.environ['PROCFG']))
    fd.write("\nexec(open(work_dir + '/etc/config.py').read())\n")
    fd.close()

exec(open(relpath + '/etc/site.py').read())
os.environ['DLC'] = dlc
os.environ['display_banner'] = 'no'
os.environ['PROTERMCAP'] = work_dir + '/etc/protermcap'
if environment == 'development':
    os.environ['PROPATH'] = ','.join(['%s/%s' % (work_dir, x) \
                      for x in modules + ['tools', 'tools/stompAdapter']]) + ',.'
    databases.extend(cdr_databases)
    del cdr_databases[:]
else:
    def modgen():
        for mod in modules:
            yield '{0}/{0}.pl'.format(mod)
            yield mod
        yield 'tools'
        yield 'tools/stompAdapter'
    os.environ['PROPATH'] = ','.join(['%s/%s' % (work_dir, x) \
                              for x in modgen()]) + ',.'
sys.path.insert(0, work_dir + '/tools')

if not 'skip_srcpkg_check' in locals() and os.path.exists(relpath + '/srcpkg'):
    srcpkg_need_update = []
    for src in os.listdir(relpath + '/srcpkg'):
        if not src.endswith('.tar'): continue
        name, version = src[:-4].split('-', 1)
        version_file = relpath + '/tools/%s/.version' % name
        if not os.path.exists(version_file) \
        or open(version_file).read().strip() != version:
            srcpkg_need_update.append(name)
    if srcpkg_need_update:
        print('WARNING: Tools %s need updating. Run `pike -C srcpkg`!' % \
                          ', '.join(srcpkg_need_update))

