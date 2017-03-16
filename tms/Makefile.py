from pike import *
from subprocess import call, Popen, PIPE
from ast import literal_eval
import tempfile
import shutil
import socket
import time
import glob
import errno

relpath = '..'
exec(open(relpath + '/etc/make_site.py').read())
if environment == 'development':
    open('Mc/version.i', 'wt').write(appversion)

myself = os.path.basename(os.getcwd())

nonp_source = ['script/' + x for x in os.listdir('script')]
skip_timelog = False
show_file = False

def userandpass():
    if 'tenancies' in globals():
        if 'tenant' in globals():
            t = tenant
        else:
            t = ''
        if t == '':
            for t, tdict in tenancies.items():
                if tdict['tenanttype'] == 'Super':
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
                if tdict['tenanttype'] == 'Super':
                    return '{0}_{1}.pf'.format(pf, temptenant)
            raise ValueError('Tenant is mandatory as a super tenant is not specified')
        elif temptenant in tenancies:
            return '{0}_{1}.pf'.format(pf, temptenant)
        else:
            raise ValueError('Unknown tenant')
    else:
        return '{}.pf'.format(pf)

def active_cdr_db_pf():
    if '-S' in open('../db/progress/store/common.pf').read():
        connection_type = "tcp"
    else:
        connection_type = "local"

    args = ['-b', '-p', 'Syst/list_active_cdr_databases.p', '-param', connection_type]
    args.extend(['-pf', getpf('../db/progress/store/common')])

    cdr_fetch = Popen(mpro + args, stdout=PIPE)
    dict = literal_eval(Popen('/bin/cat', stdin=cdr_fetch.stdout, stdout=PIPE).communicate()[0])

    if 'tenancies' in globals():
        uandp = userandpass()
        for db in dict:
            dict[db].extend(uandp)

    return dict

@target('test>test')
def test(*a): pass

@target
def daemon(*a):
    if len(parameters) < 1:
        raise PikeException('Expected daemon name and instance as parameter')
    daemon = parameters[0]
    instance = parameters[1]
    cdr_dict = {}

    if instance == "?":
        instance = ""
    pid_file = '../var/run/d-' + daemon + instance + '.pid'

    if os.path.exists(pid_file):
        print("The " + daemon + instance + " is already running")
        return

    print("Starting " + daemon + instance + "... ")
    args = mpro + ['-b', '-p', 'gearbox/daemons/run_daemon.p',
            '-clientlog', '../var/log/d-' + daemon + instance + '.log',
            '-logthreshold', '209715200', '-numlogfiles', '0',
            '-param', daemon + ',' + instance + ',../var/run',
            '-T', '../var/tmp']

    dbcount = 0
    for pp in parameters[2:]:
        if pp in databases:
            args.extend(['-pf', getpf('../db/progress/store/{0}'.format(pp))])
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
        args.extend(['-h', str(dbcount + 4)])

    daemonpf = '../etc/pf/' + daemon + '.pf'
    if os.path.exists(daemonpf):
        args.extend(pftolist(['-pf', daemonpf]))
    file = open(pid_file, 'w')
    file.write(str(os.getpid()))
    file.close()
    os.execlp(args[0], *args)

@target
def rundaemons(*a): pass

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

@target
def code_pl(*a):
    """code.pl"""
    if os.path.exists(myself + '.pl'):
        os.unlink(myself + '.pl')
    compiledir = 'temp_r'
    _compile('COMPILE %s SAVE INTO {}.'.format(compiledir), compiledir)
    os.chdir(compiledir)
    call([dlc + '/bin/prolib', '../%s.pl' % myself, '-create'])
    for dir, _dirs, files in os.walk('.'):
        for file in files:
            if file.endswith('.r'):
                call([dlc + '/bin/prolib', '../%s.pl' % myself, '-add',
                      os.path.join(dir[2:], file)])
    os.chdir('..')
    shutil.rmtree(compiledir)

@target
def compile(match, *a):
    '''compile$|compilec$|preprocess$|xref$'''

    if match == 'compile':
        compiledir = 'r'
        compilecommand = 'COMPILE %s SAVE INTO {}.'
    elif match == 'compilec':
        compiledir = None
        compilecommand = 'COMPILE %s.'
    elif match == 'preprocess':
        compiledir = 'pp'
        compilecommand = 'COMPILE %s PREPROCESS {}/%s.'
    else:
        compiledir = 'xref'
        compilecommand = 'COMPILE %s XREF {}/%s.'

    if 'dir' in globals():
        compiledir = dir

    _compile(compilecommand.format(compiledir), compiledir)

def _compile(compilecommand, compiledir):
    source_files = []

    if 'parameters' in globals() and len(parameters) > 0:
        source_files = [ filu for filu in parameters if re.search(r'.*\.(p|cls)$', filu) ]
    else:
        source_files.extend(['applhelp.p'])
        for source_dir in os.listdir('.'):
            if not os.path.isdir(source_dir) or source_dir in ['test', 'scripts', 'r', compiledir, 'pp', 'xref']:
                continue
            source_files.extend([ filu for filu in glob('{}/*'.format(source_dir)) if re.search(r'.*\.(p|cls)$', filu)] )

    if compiledir:
        seen = []
        for dir in (os.path.dirname(compiledir +'/' + x) for x in source_files):
            if dir not in seen:
               mkdir_p(dir)
               seen.append(dir)

    args = ['-pf', getpf('../db/progress/store/all')]
    cdr_dict = {}

    for cdr_database in cdr_databases:
        if not cdr_dict:
            cdr_dict = active_cdr_db_pf()
        if cdr_database in cdr_dict:
            args.extend(cdr_dict[cdr_database])

    procedure_file = make_compiler(compilecommand, source_files, show='name' if show_file else '.')

    if environment == 'safeproduction':
        os.environ['PROPATH'] = os.environ['PROPATH'].split(',', 1)[1]

    comp = Popen(mpro + args + ['-b', '-inp', '200000', '-tok', '20000', '-p', procedure_file.name], stdout=PIPE)
    call('/bin/cat', stdin=comp.stdout)
    if comp.wait() != 0:
        raise PikeException('Compilation failed')

    print('')

def mkdir_p(directory):
    try:
        os.makedirs(directory)
    except OSError as exc:  # Python >2.5
        if exc.errno == errno.EEXIST and os.path.isdir(directory):
            pass
        else:
            raise

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

@target
def cui(*a):
    '''cui|vim|vimbatch'''

    args = ['-pf', getpf('../db/progress/store/all')]

    if a[0] == 'cui':
        program = 'Syst/tmslogin.p'
        args.extend(['-clientlog', '../var/log/tms_ui.log', '-logginglevel', '4'])
    else: # Only vim should use this block internally...
        if len(parameters) == 0:
            raise PikeException('Expected a module to run as a parameter')
        if a[0] == 'vimbatch':
            args.extend(['-b'])
        program = parameters[0]

    cdr_dict = {}
    for cdr_database in cdr_databases:
        if not cdr_dict:
            cdr_dict = active_cdr_db_pf()
        if cdr_database in cdr_dict:
            args.extend(cdr_dict[cdr_database])

    args.extend(['-T', '../var/tmp', '-p', program])

    cmd = Popen(mpro + args)
    while cmd.poll() is None:
        try:
            cmd.wait()
        except KeyboardInterrupt:
            cmd.send_signal(2)
    sys.exit(cmd.returncode)

@target
def terminal(*a):
    args = []

    if len(parameters) == 0:
        raise PikeException('Expected a module to run as a parameter')

    cdr_dict = {}
    all_in_parameters = False

    if 'all' in parameters[1:]:
        all_in_parameters = True
        args.extend(['-pf', getpf('../db/progress/store/all')])
        cdr_dict = active_cdr_db_pf()
        for db in cdr_dict:
            args.extend(cdr_dict[db])

    dbcount = 0
    for pp in parameters[1:]:
        if pp in databases:
            if all_in_parameters:
                continue
            args.extend(['-pf', getpf('../db/progress/store/{0}'.format(pp))])
            dbcount += 1
        elif pp in cdr_databases:
            if all_in_parameters:
                continue
            if not cdr_dict:
                cdr_dict = active_cdr_db_pf()            
            if pp in cdr_dict:
                args.extend(cdr_dict[pp])
                dbcount += 1
        elif pp != 'all':
            args.append(pp)

    args.extend(['-T', '../var/tmp', '-p', parameters[0]])

    if dbcount != 0:
        args.extend(['-h', str(dbcount + 4)])

    cmd = Popen(mpro + args)
    while cmd.poll() is None:
        try:
            cmd.wait()
        except KeyboardInterrupt:
            cmd.send_signal(2)
    sys.exit(cmd.returncode)

@target
def batch(*a):
    assert len(parameters) > 0, 'Which module to run?'
    batch_module = parameters[0] 
    module_base = os.path.basename(batch_module)
    cdr_dict = {}

    if os.path.exists('../var/run/%s.pid' % module_base):
        print('Lockfile %s.pid exists - aborting!' % module_base)
        sys.exit(5)
    fd = open('../var/run/%s.pid' % module_base, 'w')
    fd.write(str(os.getpid()))
    fd.close()

    args = ['-T', '../var/tmp', '-b', '-p', batch_module + '.p']

    all_in_parameters = False
    if 'all' in parameters[1:]:
        all_in_parameters = True
        args.extend(['-pf', getpf('../db/progress/store/all')])
        cdr_dict = active_cdr_db_pf()
        for db in cdr_dict:
            args.extend(cdr_dict[db])

    dbcount = 0
    for pp in parameters[1:]:
        if pp in databases:
            if all_in_parameters:
                continue
            args.extend(['-pf', getpf('../db/progress/store/{0}'.format(pp))])
            dbcount += 1
        elif pp in cdr_databases:
            if all_in_parameters:
                continue
            if not cdr_dict:
                cdr_dict = active_cdr_db_pf()
            if pp in cdr_dict:
                args.extend(cdr_dict[pp])
                dbcount += 1
        elif pp != 'all':
            args.append(pp)

    if dbcount != 0:
        args.extend(['-h', str(dbcount + 4)])

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
    cdr_dict = {}

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

    all_in_parameters = False
    if 'all' in parameters[1:]:
        all_in_parameters = True
        args.extend(['-pf', getpf('../db/progress/store/all')])
        cdr_dict = active_cdr_db_pf()
        for db in cdr_dict:
            args.extend(cdr_dict[db])

    dbcount = 0
    for pp in parameters[2:]:
        if pp in databases:
            if all_in_parameters:
                continue
            args.extend(['-pf', getpf('../db/progress/store/{0}'.format(pp))])
            dbcount += 1
        elif pp in cdr_databases:
            if all_in_parameters:
                continue
            if not cdr_dict:
                cdr_dict = active_cdr_db_pf()
            if pp in cdr_dict:
                args.extend(cdr_dict[pp])
                dbcount += 1
        elif pp != 'all':
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

    if dbcount != 0:
        args.extend(['-h', str(dbcount + 4)])

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
    '''editor||yeditor||meditor'''
    if not 'tenant' in globals():
        global tenant
        tenant = ''

    if a[0] == 'yeditor':
        tenant = "yoigo"
    elif a[0] == 'meditor':
        tenant = "masmovil"

    args = parameters or (['-pf', getpf('../db/progress/store/all')])
    args = mpro + args + ['-T', '../var/tmp', '-clientlog', '../var/log/tms_editor.log', '-logginglevel', '4']
    os.execlp(args[0], *args)
