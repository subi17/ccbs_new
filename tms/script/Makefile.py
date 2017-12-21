from pike import *
#import thread
import os
import sys
import shutil
import json
import errno
import time
import threading
from socket import gethostname, error
from subprocess import Popen

relpath = '../..'
exec(open(relpath + '/etc/make_site.py').read())

state_base = os.path.abspath(os.path.join(relpath, 'var', 'run')) + '/'


if environment == 'slavedevelopment':
    configfile = 'slavedevelopment_config.json'
elif environment == 'development' and not os.path.exists(gethostname() + '_config.json'):
    configfile = 'development_config.json'
else:
    configfile = gethostname() + '_config.json'

if not os.path.exists(configfile):
    raise PikeException('Failed, no daemon configuration file ' + configfile + ' found')

configfile = os.path.abspath(configfile)

default_databases = ('common', 'ordercanal', 'mobile', 'star', 'counter', 'reratelog')

threads = []

def pid_exists(pid):
    """Check whether pid exists in the current process table.
    UNIX only.
    """
    if pid < 0:
        return False
    if pid == 0:
        # According to "man 2 kill" PID 0 refers to every process
        # in the process group of the calling process.
        # On certain systems 0 is a valid PID but we have no way
        # to know that in a portable fashion.
        raise ValueError('invalid PID 0')
    try:
        os.kill(pid, 0)
    except OSError as err:
        if err.errno == errno.ESRCH:
            # ESRCH == No such process
            return False
        elif err.errno == errno.EPERM:
            # EPERM clearly means there's a process to deny access to
            return True
        else:
            # According to "man 2 kill" possible error values are
            # (EINVAL, EPERM, ESRCH)
            raise
    else:
        return True

def userandpass():
    if 'tenancies' in globals():
        if 'tenant' in globals():
            t = tenant
        else:
            t = ''
        if t == '':
            for t, tdict in tenancies.items():
                if tdict['tenanttype'] == 'Super' or len(tenancies) == 1:
                    return ['-U', '{0}@{1}'.format(tdict['username'], tdict['domain']), '-P', tdict['password'] ]
            raise ValueError('Tenant is mandatory as a super tenant is not specified')
        elif t in tenancies:
            return ['-U', '{0}@{1}'.format(tenancies[t]['username'], tenancies[t]['domain']), '-P', tenancies[t]['password'] ]
        else:
            raise ValueError('Unknown tenant')
    else:
        return []

def pftolist(pf):

    if not 'tenancies' in globals():
        return pf

    pffile = pf[1]

    uandp = userandpass()

    with open(pffile, 'r') as myfile:
        data = myfile.read().splitlines()

    returndata = []

    for line in data:
        for word in line.split():
            returndata.append(word)
            if word.endswith('.pf'):
                returndata.extend(uandp)

        if line.startswith('-db'):
            returndata.extend(uandp)

    return returndata

def getpf(pf):
    if 'tenancies' in globals():
        if 'tenant' in globals():
            temptenant = tenant
        else:
            temptenant = ''

        if temptenant == '':
            for temptenant, tdict in tenancies.items():
                if tdict['tenanttype'] == 'Super' or len(tenancies) == 1:
                    return '{0}_{1}.pf'.format(pf, temptenant)
            raise ValueError('Tenant is mandatory as a super tenant is not specified')
        elif temptenant in tenancies:
            return '{0}_{1}.pf'.format(pf, temptenant)
        else:
            raise ValueError('Unknown tenant')
    else:
        return '{0}.pf'.format(pf)

def active_cdr_db_pf():
    if '-S' in open('{0}/db/progress/store/common.pf'.format(relpath)).read():
        connection_type = "tcp"
    else:
        connection_type = "local"

    if 'dbstate' in globals():
        connection_type += ',{0}'.format(dbstate)

    args = ['-b', '-p', 'Syst/list_active_cdr_databases.p', '-param', connection_type]
    args.extend(['-pf', getpf('../db/progress/store/common'), '-h', '1'])

    cdr_fetch = Popen(mpro + args, stdout=PIPE)
    dict = literal_eval(cdr_fetch.communicate()[0])

    if 'tenancies' in globals():
        uandp = userandpass()
        for db in dict:
            dict[db].extend(uandp)

    return dict

def param_gen(a):
   for item in a:
       yield item.partition(':')

def daemon_gen(outputmode, a):
    daemondict = {}
    for item in [param for param in param_gen(a)]:
        if item[0] in daemondict:
            daemondict[item[0]].append(item[2].lower())
        else:
            daemondict[item[0]] = [item[2].lower()]

    with open(configfile, 'r') as jsonfile:
        jsondata = json.load(jsonfile)
        for daemon in jsondata:
            for daemonnick in jsondata[daemon]:
                if not daemondict or daemonnick in daemondict:
                    for instance in jsondata[daemon][daemonnick]:
                        if not daemondict or '' in daemondict[daemonnick] or instance.lower() in daemondict[daemonnick]:
                            if outputmode == 'just_name':
                                yield 'd-{0}{1}'.format(daemon, instance)
                            elif outputmode == 'print':
                                yield ('d-{0}{1}'.format(daemon, instance),) + (format(daemonnick, '<15') + '| ' + format(instance.lower(), '<15') + '| ' + format('d-{0}{1}'.format(daemon, instance), '<36') +  '| ',)
                            elif outputmode == 'detail':
                                if not 'databases' in jsondata[daemon][daemonnick][instance]:
                                    databases = default_databases
                                else:
                                    databases = tuple(jsondata[daemon][daemonnick][instance]['databases'].split(' '))
                                    del jsondata[daemon][daemonnick][instance]['databases']
                                yield ('d-{0}{1}'.format(daemon, instance),) + (daemon,) + (instance,) + databases + tuple(x for t in jsondata[daemon][daemonnick][instance].items() for x in t)
        jsonfile.close()

@target
def start(*a):
    '''start|run|rundaemons'''
    
    os.chdir('..')
    for daemonitem in daemon_gen('detail', parameters):

        pid_file = state_base + daemonitem[0] + '.pid'
        if os.path.exists(pid_file):
            print('No need to start already running daemon ' + daemonitem[0])
            continue

        # daemon = daemonitem[1]
        # instance = daemonitem[2]
        cdr_dict = {}
        
        print("Starting " + daemonitem[0] + "... ")
        args = mpro + ['-b', '-p', 'gearbox/daemons/run_daemon.p',
                '-clientlog', '{0}/var/log/{1}.log'.format(relpath[:-3], daemonitem[0]),
                '-logthreshold', '209715200', '-numlogfiles', '0',
                '-param', daemonitem[1] + ',' + daemonitem[2] + ',../var/run']

        dbcount = 0
        for pp in daemonitem[3:]:
            if pp in databases:
                args.extend(['-pf', getpf('{0}/db/progress/store/{1}'.format(relpath[:-3], pp))])
                dbcount += 1
            elif pp in cdr_databases:
                if not cdr_dict:
                    cdr_dict = active_cdr_db_pf()
                if pp in cdr_dict:
                    args.extend(cdr_dict[pp])
                    dbcount += 1
            else:
                args.append(pp)

        if dbcount != 0:
            args.extend(['-h', str(dbcount + cdr_database_count * 2)])

        daemonpf = '{0}/etc/pf/{1}.pf'.format(relpath[:-3], daemonitem[1])
        if os.path.exists(daemonpf):
            args.extend(pftolist(['-pf', daemonpf]))

        pid = Popen(args).pid

        file = open(pid_file, 'w')
        file.write(str(pid))
        file.close()
        
        if not pid_exists(pid):
            print('FAILED to start daemon ' + daemonitem[0])


def worker(pid, daemonname):
    """thread worker function"""
    count = 0
    for waittime in (1, 1, 1, 1, 1, 2, 3, 5, 7, 7, 7, 7, 7, 7, 5, 5, 3, 2, 2):
        if not pid_exists(pid):
            break
        time.sleep(waittime)
        count += waittime
        if count > 5:
            print('Waiting daemon {0} (PID={1}) to stop. Waited so far {2} seconds...'.format(daemonname, pid, count))

    if pid_exists(pid):
        print('After waiting {0} seconds daemon {1} is still running with PID {2}. You might need to kill it manually (remember to delete the die file if doing this).'.format(daemonname, pid, count))
    else:
        print('Stopped daemon {0}.'.format(daemonname))

class myThread (threading.Thread):
   def __init__(self, pid, daemonname):
      threading.Thread.__init__(self)
      self.pid = pid
      self.daemonname = daemonname
   def run(self):
      # print "Stopping daemon " + self.daemonname
      worker(self.pid, self.daemonname)

@target
def stop(*a):
    '''stop|stopdaemons'''
    daemonsavailable = False
    for daemonitem in daemon_gen('just_name', parameters):
        pidfile = state_base + daemonitem + '.pid'
        if os.path.exists(pidfile):
            daemonsavailable = True
            fd = open(pidfile, 'rt')
            pid = int(fd.read().strip())
            fd.close()

            if pid_exists(pid):
                open(state_base + daemonitem + '.die', 'a').close()
                thread = myThread(pid, daemonitem)
                thread.start()
                threads.append(thread)
            else:
                print("Daemon process marked to pid file {0} doesn't exists! Deleted the pid file.".format(pidfile))
                os.unlink(pidfile)

    if not daemonsavailable and not parameters:
        print('No daemons are running.')

    # Wait for all threads to complete
    for t in threads:
        t.join()

@target
def status(*a):
    '''default|status|daemonsstatus|daemonstatus'''
    problemlist = []
    header = True
    for daemonitem in daemon_gen('print', parameters):
        pid = "No"
        pidfile = state_base + daemonitem[0] + '.pid'
        if os.path.exists(pidfile):
            fd = open(pidfile, 'rt')
            pid = fd.read().strip()
            fd.close()

            if not pid_exists(int(pid)):
                pid = "No"
                problemlist.append(pidfile)

        if header:
            print('Category       | Instance       | Daemon name                         | Running')
            print('-------------------------------------------------------------------------------')
            header = False

        print(daemonitem[1] + pid)

    print

    for pidfile in problemlist:
        print("Daemon process marked to pid file {0} doesn't exists! Deleted the pid file.".format(pidfile))
        os.unlink(pidfile)