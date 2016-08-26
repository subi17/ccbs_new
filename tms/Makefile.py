from pike import *
from subprocess import call, Popen, PIPE
from ast import literal_eval
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

# Leto ei toimi, kun tarvii tcp connectoinnin. Kuinka hoidetaan???
def active_cdr_db_pf():
    cdr_fetch = Popen(mpro + ['-pf', '../db/progress/store/common.pf',
                              '-b', '-p', Syst/list_active_cdr_databases.p], stdout=PIPE)
    return literal_eval(Popen('/bin/cat', stdin=cdr_fetch.stdout, stdout=PIPE).communicate()[0])

cdr_databases = active_cdr_db_pf()

@target('test>test')
def test(*a): pass

@target
def daemon(*a):
    if len(parameters) < 1:
        raise PikeException('Expected daemon name and instance as parameter')
    daemon = parameters[0]
    instance = parameters[1]
    if instance == "?":
        instance = ""
    pid_file = '../var/run/d-' + daemon + instance + '.pid'

    if os.path.exists(pid_file):
        print("The " + daemon + instance + " is already running")
        return

    print("Starting " + daemon + instance + "... ")
    args = mpro + ['-b', '-p', 'gearbox/daemons/run_daemon.p',
            '-clientlog', '../var/log/d-' + daemon + instance + '.log',
            '-param', daemon + ',' + instance + ',../var/run',
            '-T', '../var/tmp']
    for pp in parameters[2:]:
        if pp in databases:
            args.extend(['-pf', '../db/progress/store/{0}.pf'.format(pp)])
        elif pp in cdr_databases
            args.extend(cdr_databases[pp])
        else:
            args.append(pp)
    daemonpf = '../etc/pf/' + daemon + '.pf'
    if os.path.exists(daemonpf):
        args.extend([ '-pf' , daemonpf ])
    file = open(pid_file, 'w')
    file.write(str(os.getpid()))
    file.close()
    os.execlp(args[0], *args)

@target
def build(*a):
    if len(parameters) != 1:
        raise PikeException('Expected build_dir as parameter')
    build_dir = parameters[0]
    for file in nonp_source + ['Makefile.py']:
        mkdir_p(os.path.dirname(os.path.join(build_dir, file)))
        shutil.copy2(file, os.path.join(build_dir, file))
    require('code.pl', [])
    shutil.move(myself + '.pl', build_dir + '/' + myself + '.pl')

applhelp_dir = 'Help'

@target('r')
def code_pl(*a):
    """code.pl"""
    if os.path.exists(myself + '.pl'):
        os.unlink(myself + '.pl')
    os.chdir('r')
    if applhelp_dir:
        os.rename(applhelp_dir + '/applhelp.r', 'applhelp.r')
    call([dlc + '/bin/prolib', '../%s.pl' % myself, '-create'])
    for dir, _dirs, files in os.walk('.'):
        for file in files:
            if file.endswith('.r'):
                call([dlc + '/bin/prolib', '../%s.pl' % myself, '-add',
                      os.path.join(dir[2:], file)])
    os.chdir('..')
    shutil.rmtree('r')

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

        comp = Popen(mpro + ['-pf', '../db/progress/store/all.pf', '-s', '1024',
                             '-b', '-p', procedure_file.name], stdout=PIPE)
        call('/bin/cat', stdin=comp.stdout)
        if comp.wait() != 0:
            raise PikeException('Compilation failed')
    print('')

@target
def compile_one(match, *a):
    '''(.*)\.(p|cls)'''
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


run_module = 'Syst/tmslogin'
run_extraargs = ['-e', '100', '-l', '2000', '-TB', '31', '-TM', '32',
                 '-rand', '2', '-Bt', '2500']
@target
def run(*a):
    args = ['-pf', '../db/progress/store/all.pf']
    args.extend(['-T', '../var/tmp'])
    args = mpro + args + run_extraargs + ['-clientlog', '../var/log/tms_ui.log', '-logginglevel', '4'] + ['-p', run_module + '.p']
    os.execlp(args[0], *args)

@target
def terminal(*a):
    assert len(parameters) > 0, 'Which module to run?'
    terminal_module = parameters[0]
    module_base = os.path.basename(terminal_module)
    if os.path.exists('../var/run/%s.pid' % module_base):
        print('Lockfile %s.pid exists - aborting!' % module_base)
        sys.exit(5)
    fd = open('../var/run/%s.pid' % module_base, 'w')
    fd.write(str(os.getpid()))
    fd.close()

    args = ['-T', '../var/tmp', '-p', terminal_module + '.p']
    for pp in parameters[1:]:
        if pp in databases:
            args.extend(['-pf', '../db/progress/store/{0}.pf'.format(pp)])
        elif pp in cdr_databases
            args.extend(cdr_databases[pp])
        else:
            args.append(pp)
    cmd = Popen(mpro + args)
    while cmd.poll() is None:
        try:
            cmd.wait()
        except KeyboardInterrupt:
            cmd.send_signal(2)
    os.unlink('../var/run/%s.pid' % module_base)
    sys.exit(cmd.returncode)

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
        elif pp in cdr_databases
            args.extend(cdr_databases[pp])
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
def idbatch(*a):
    assert len(parameters) > 0, 'Which module to run?'
    batch_module = parameters[0]
    module_base = os.path.basename(batch_module)
    try:
        batchid = int(parameters[1])
        if batchid in databases:
            raise IndexError
    except ValueError:
        print('Batch ID must be a number - aborting!')
        sys.exit(5)
    except IndexError:
        print('No batch ID given - aborting!')
        sys.exit(5)
     
    if os.path.exists('../var/run/%s_%s.pid' % (module_base, batchid)):
        print('Lockfile %s_%s.pid exists - aborting!' % (module_base, batchid))
        sys.exit(5)
    fd = open('../var/run/%s_%s.pid' % (module_base, batchid), 'w')
    fd.write(str(os.getpid()))
    fd.close()

    args = ['-T', '../var/tmp', '-b', '-p', batch_module + '.p']
    for pp in parameters[2:]:
        if pp in databases:
            args.extend(['-pf', '../db/progress/store/{0}.pf'.format(pp)])
        elif pp in cdr_databases
            args.extend(cdr_databases[pp])
        else:
            args.append(pp)
    try:
        idx = args.index("-param")
    except ValueError:
        args.extend(["-param", 'batchid=%s' % batchid])
    else:
        try:
            args[idx + 1] = 'batchid=%s,%s' % (batchid, args[idx + 1])
        except IndexError:
            args.append('batchid=%s' % batchid)

    logfile = open('../var/log/%s_%s.log' % (module_base, batchid), 'a')
    if not skip_timelog:
        logfile.write(time.strftime('%F %T %Z') + ' {0}\n'.format('='*50))
        logfile.flush()
    cmd = Popen(mpro + args, stdout=logfile)
    while cmd.poll() is None:
        try:
            cmd.wait()
        except KeyboardInterrupt:
            cmd.send_signal(2)
    os.unlink('../var/run/%s_%s.pid' % (module_base, batchid))
    sys.exit(cmd.returncode)

@target
def editor(*a):
    args = parameters or ['-pf', '../db/progress/store/all.pf']
    args = mpro + args + ['-T', '../var/tmp'] + ['-s', '1024'] + ['-clientlog', '../var/log/tms_editor.log', '-logginglevel', '4']
    os.execlp(args[0], *args)
