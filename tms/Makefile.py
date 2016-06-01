from pike import *
from subprocess import call, Popen, PIPE
import tempfile
import shutil
import socket
import time
import glob

relpath = '..'
exec(open(relpath + '/etc/make_site.py').read())
if environment == 'development':
    open('Mc/version.i', 'wt').write(appversion)

myself = os.path.basename(os.getcwd())

nonp_source = ['script/' + x for x in os.listdir('script')]
skip_timelog = False
show_file = False

log_level = '2'

# example: log_entry_types = '4glmessages,4gltrace'
log_entry_types = '4glmessages'
daemon_state_dir = '../var/run'

daemon_dirs = ['core/daemons']

@target('test>test')
def test(*a): pass


def check_daemon(daemon):
    return os.path.isfile(daemon.replace('.','/') + '.cls')

def pid_file(daemon):
    return daemon_state_dir+"/d-"+daemon+".pid"

def start_daemon(daemon):

    if not check_daemon(daemon):
        print "That daemon ("+daemon+") does not exist"
        return

    if os.path.isfile(pid_file(daemon)):
        print "The daemon "+daemon+" is already running"
        return

    print "Starting "+daemon+"... "
    file = open(pid_file(daemon), 'w')
    file.write(str(os.getpid()))
    file.close()
    os.system(mpro[0] +' -b -p gearbox/daemons/run_daemon.p' +
              ' -clientlog ../var/log/d-'+daemon+'.log' +
              ' -pf ../etc/pf/d-'+daemon+'.pf' +
              ' -logginglevel '+log_level+' -logentrytypes '+log_entry_types +
              ' -param '+daemon+',,'+daemon_state_dir +
              ' >> ../var/log/d-'+daemon+'-console.log &')
    time.sleep(1)
    if not os.path.isfile(pid_file(daemon)):
        raise PikeException("** DAEMON "+daemon+" did not start.")


def stop_daemon(daemon):

    pidfile = pid_file(daemon)

    if not check_daemon(daemon):
        print "That daemon ("+daemon+") does not exist"
        return
    if not os.path.isfile(pidfile):
        print "The daemon "+daemon+" is not running"
        return
    file = open(daemon_state_dir+"/d-"+daemon+".die", 'w')
    file.write("")
    file.close()

    file = open(pidfile,"r")
    pid = file.read()
    file.close()

    for i in [1, 2, 3, 2, 2]:
        time.sleep(i)
        if os.system('ps -p '+pid+ ">/dev/null"):
            break

    if not os.system('ps -p '+pid+ ">/dev/null"):
        raise PikeException("** DAEMON "+daemon+" seems to hang. Try killing it")

    print("The daemon "+daemon+" was stopped successfully.")

def list_all_daemons():
    for dir in daemon_dirs:
        for daemon in glob.iglob(dir+"/*.cls"):
            yield daemon.replace('/','.').replace('.cls','')

@target
def start(*a):
    target = 'all'
    if len(parameters) == 1:
        target = parameters[0]

    if target == 'all':
        for daemon in list_all_daemons():
            start_daemon(daemon)
    else:
        start_daemon(target)

@target
def stop(*a):
    target = 'all'
    if len(parameters) == 1:
        target = parameters[0]

    if target == 'all':
        for daemon in list_all_daemons():
            stop_daemon(daemon)
    else:
        stop_daemon(target)

@target
def build(*a):
    if len(parameters) != 1:
        raise PikeException('Expected build_dir as parameter')
    open('misc/version.i', 'wt').write(appversion)
    build_dir = parameters[0]
    for file in nonp_source + ['Makefile.py']:
        mkdir_p(os.path.dirname(os.path.join(build_dir, file)))
        shutil.copy2(file, os.path.join(build_dir, file))
    require('code.pl', [])
    shutil.move(myself + '.pl', build_dir + '/' + myself + '.pl')

applhelp_dir = None

def write_dirstate(filename):
    info_file = open(filename, 'wt')
    call(['hg', 'summary'], stdout=info_file)
    call(['hg', 'st'], stdout=info_file)
    call(['hg', 'diff'], stdout=info_file)
    info_file.close()

@target('r')
def code_pl(*a):
    """code.pl"""
    if os.path.exists(myself + '.pl'):
        os.unlink(myself + '.pl')
    os.chdir('r')
    write_dirstate('distinfo')
    if applhelp_dir:
        os.rename(applhelp_dir + '/applhelp.r', 'applhelp.r')
    call([dlc + '/bin/prolib', '../%s.pl' % myself, '-create'])
    for dir, _dirs, files in os.walk('.'):
        for file in files:
            if file.endswith('.r') or file == 'distinfo':
                call([dlc + '/bin/prolib', '../%s.pl' % myself, '-add',
                      os.path.join(dir[2:], file)])
    os.chdir('..')
    shutil.rmtree('r')

@target
def hotfix(*a):
    inst_dir = '/apps' if len(parameters) != 1 else parameters[0]
    inst_dir += '/' + appname + "-" + appversion
    proc = Popen(['hg', 'branch'], stdout=PIPE)
    branch = proc.communicate()[0].strip()
    if branch != 'default' and not branch.endswith('-hotfix'):
        inst_dir += '-' + branch
    print('Hot-fixing ' + inst_dir)
    open('misc/version.i', 'wt').write(appversion)
    require('code.pl')
    write_dirstate(inst_dir + '/var/log/hotfix.log')
    os.rename(myself + '.pl', os.path.join(inst_dir, myself, myself + '.pl'))

@target
def r(*a):
    '''r$'''
    if os.path.exists('r'):
        shutil.rmtree('r')
    os.mkdir('r')
    require('compile', ['r'])

@target
def compile(*a):
    global parameters
    if len(parameters) == 1:
        target = parameters[0]
        directive = ' SAVE INTO ' + parameters[0]
        parameters = []
    else:
        target = None
        directive = ''
    _compile_all('COMPILE %%s%s.' % directive, target)

@target
def preprocess(*a):
    global parameters
    if len(parameters) == 1:
        target = parameters[0]
        parameters = []
    else:
        target = 'pp'
    _compile_all('COMPILE %%s PREPROCESS %s/%%s.' % target, target)

@target
def xref(*a):
    global parameters
    if len(parameters) == 1:
        target = parameters[0]
        parameters = []
    else:
        target = 'xref'
    _compile_all('COMPILE %%s XREF %s/%%s.' % target, target)

def classes_and_procedures(directory):
    for dir, _dirs, files in os.walk(directory):
        for file in files:
            if file.endswith('.p') or file.endswith('.cls'):
                yield os.path.join(dir, file)

def _compile_all(format, target):
    for source_dir in os.listdir('.'):
        if not os.path.isdir(source_dir) \
        or source_dir in ['test', 'scripts']:
            continue
        source_files = list(classes_and_procedures(source_dir))
        procedure_file = make_compiler(format, source_files,
                                       show='name' if show_file else '.')
        if target:
            seen = []
            for dir in (os.path.dirname(target +'/' + x) for x in source_files):
                if dir not in seen:
                    mkdir_p(dir)
                    seen.append(dir)

        comp = Popen(mpro + ['-pf', '../db/progress/store/all.pf',
                             '-b', '-p', procedure_file.name], stdout=PIPE)
        call('/bin/cat', stdin=comp.stdout)
        if comp.wait() != 0:
            raise PikeException('Compilation failed')
    print('')

@target
def compile_one(match, *a):
    '''(.*)\.p'''
    print('Compiling file ' + match)
    sys.stdout.flush()
    procedure_file = make_compiler('COMPILE %s.', [match], show=None)
    comp = Popen(mpro + ['-pf', '../db/progress/store/all.pf',
                         '-b', '-p', procedure_file.name], stdout=PIPE)
    call('/bin/cat', stdin=comp.stdout)
    if comp.wait() != 0:
        raise PikeException('Compilation failed')

def mkdir_p(directory):
    direntries = directory.split(os.path.sep)
    for dir in [os.path.sep.join(direntries[:ii+1]) \
                for ii in range(len(direntries))]:
        if dir and not os.path.exists(dir):
            os.mkdir(dir)

def make_compiler(cline, files, show='.'):
    compiler = tempfile.NamedTemporaryFile(suffix='.p', mode='wt+')
    compiler.write('ROUTINE-LEVEL ON ERROR UNDO, THROW.\n')
    for ff in files:
        if show == '.':
            compiler.write('PUT UNFORMATTED \'.\'.\n')
        elif show == 'name':
            compiler.write('PUT UNFORMATTED \'%s~n\'.\n' % ff)
        compiler.write(cline.replace('%s', ff) + '\n')
    compiler.flush()
    return compiler

@target
def clean(*a):
    for file in glob('procore*') + glob(myself + '.pl'):
        os.unlink(file)


run_module = 'misc/nn'
run_extraargs = ['-e', '100', '-l', '2000', '-TB', '31', '-TM', '32',
                 '-rand', '2', '-Bt', '2500']
@target
def run(*a):
    args = ['-pf', '../db/progress/store/all.pf']
    args.extend(['-T', '../var/tmp'])
    args = mpro + args + run_extraargs + ['-p', run_module + '.p']
    os.execlp(args[0], *args)

@target
def batch(*a):
    assert len(parameters) > 0, 'Which module to run?'
    batch_module = parameters[0] 
    module_base = os.path.basename(batch_module)
    if os.path.exists('../var/run/%s.pid' % module_base):
        print('Lockfile %s.pid exists - aborting!' % module_base)
        sys.exit(5)
    fd = open('../var/run/%s.pid' % module_base, 'w')
    fd.write(str(os.getpid()))
    fd.close()

    args = ['-T', '../var/tmp', '-b', '-p', batch_module + '.p']
    for pp in parameters[1:]:
        if pp in databases:
            args.extend(['-pf', '../db/progress/store/{0}.pf'.format(pp)])
        else:
            args.append(pp)
    logfile = open('../var/log/%s.log' % module_base, 'a')
    if not skip_timelog:
        logfile.write(time.strftime('%F %T %Z') + ' {0}\n'.format('='*50))
        logfile.flush()
    cmd = Popen(mpro + args, stdout=logfile)
    while cmd.poll() is None:
        try:
            cmd.wait()
        except KeyboardInterrupt:
            cmd.send_signal(2)
    os.unlink('../var/run/%s.pid' % module_base)
    sys.exit(cmd.returncode)

@target
def editor(*a):
    args = parameters or ['-pf', '../db/progress/store/all.pf']
    args = mpro + args + ['-T', '../var/tmp']
    os.execlp(args[0], *args)
