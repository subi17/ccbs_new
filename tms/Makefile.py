from pike import *
from subprocess import call, Popen, PIPE
from socket import gethostname
from ast import literal_eval
import tempfile
import shutil
import time
import glob
import errno
import resource

relpath = '..'
exec(open(relpath + '/etc/make_site.py').read())

myself = os.path.basename(os.getcwd())

nonp_source = ['script/' + x for x in os.listdir('script')]
skip_timelog = False
show_file = False

# Setrlimit will do same command as "ulimit -c 4000"
_, hardlimit = resource.getrlimit(resource.RLIMIT_CORE)
if hardlimit == resource.RLIM_INFINITY or hardlimit >= 4096000:
    resource.setrlimit(resource.RLIMIT_CORE,(4096000, hardlimit))

if 'umask' in globals():
    os.umask(int(umask))

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

    if instance == "__NO__":
        instance = ""
    pid_file = '../var/run/d-' + daemon + instance + '.pid'

    if os.path.exists(pid_file):
        print("The " + daemon + instance + " is already running")
        return

    print("Starting " + daemon + instance + "... ")
    args = mpro + ['-b', '-p', 'gearbox/daemons/run_daemon.p',
            '-clientlog', '../var/log/d-' + daemon + instance + '.log',
            '-logthreshold', '209715200', '-numlogfiles', '0',
            '-param', daemon + ',' + instance + ',../var/run']

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
def build(match, *a):
    '''build|buildextapi'''
    if len(parameters) != 1:
        raise PikeException('Expected build_dir as parameter')
    build_dir = parameters[0]
    mkdir_p(build_dir)

    if match == 'build':
        for file in nonp_source + ['Makefile.py']:
            mkdir_p(os.path.dirname(os.path.join(build_dir, file)))
            shutil.copy2(file, os.path.join(build_dir, file))
        require('compile.and.do.pl', [])
    else:
        print('Using /tmsapps r-files. Please make sure that you have compiled them!')
        do_pl('/tmsapps')

    shutil.move(myself + '.pl', build_dir + '/' + myself + '.pl')

def do_pl(rdir):
    currdir = os.getcwd()
    plfile = '{0}/{1}.pl'.format(currdir, myself)

    if os.path.exists(plfile):
        os.unlink(plfile)

    os.chdir(rdir)
    call([dlc + '/bin/prolib', plfile, '-create'])
    for dir, _dirs, files in os.walk('.'):
        for file in files:
            if file.endswith('.r'):
                call([dlc + '/bin/prolib', plfile, '-add',
                      os.path.join(dir[2:], file)])
    os.chdir(currdir)

@target
def compile_and_do_pl(*a):
    '''compile.and.do.pl'''
    compiledir = 'temp_r'
    _compile('COMPILE %s SAVE INTO {0}.'.format(compiledir), compiledir)
    do_pl(compiledir)
    shutil.rmtree(compiledir)

@target
def compile(match, *a):
    '''compile$|compilec$|preprocess$|xref$'''

    if match == 'compile':
        if environment == 'safeproduction':
            compiledir = '/tmsapps'
        else:
            compiledir = 'r'
        compilecommand = 'COMPILE %s SAVE INTO {0} NO-ERROR.'
    elif match == 'compilec':
        compiledir = None
        compilecommand = 'COMPILE %s NO-ERROR.'
    elif match == 'preprocess':
        compiledir = 'pp'
        compilecommand = 'COMPILE %s PREPROCESS {0}/%s NO-ERROR.'
    else:
        compiledir = 'xref'
        compilecommand = 'COMPILE %s XREF {0}/%s NO-ERROR.'

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
            if not os.path.isdir(source_dir) or source_dir in ['test', 'scripts', 'r', 'newdf', compiledir, 'pp', 'xref']:
                continue
            source_files.extend([ filu for filu in glob('{0}/*'.format(source_dir)) if re.search(r'.*\.(p|cls)$', filu)] )

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

    compile_p = make_compiler(compilecommand, source_files, show='name' if show_file else '.')

    if environment == 'safeproduction':
        os.environ['PROPATH'] = os.environ['PROPATH'].split(',', 1)[1]

    if os.path.isfile('{0}/progress.cfg.edit'.format(dlc)):
        os.environ['PROCFG'] = '{0}/progress.cfg.edit'.format(dlc)

    processes = []
    for file in compile_p:
        comp = Popen(mpro + args + ['-b', '-inp', '200000', '-tok', '20000', '-p', file], stdout=PIPE)
        processes.append(comp)

    comp_error = False
    for comp in processes:
        call('/bin/cat', stdin=comp.stdout)
        comp.wait()

    for file in compile_p:
        os.unlink(file)

    print('')

def mkdir_p(directory):
    try:
        os.makedirs(directory)
    except OSError as exc:  # Python >2.5
        if exc.errno == errno.EEXIST and os.path.isdir(directory):
            pass
        else:
            raise

def chunks(l, n):
    """Yield successive n-sized chunks from l."""
    for i in xrange(0, len(l), n):
        yield l[i:i + n]

def make_compiler(cline, files, show='.'):
    if not 'multi' in globals():
        global multi
        multi = 1
    else:
        if multi.isdigit() == False or int(multi) > 16 or int(multi) < 1:
            raise PikeException('multi must be integer from 1 to 16')
        multi = int(multi)

    compiler_p_list = []
    for subfiles in chunks(files, (len(files) + multi - 1) // multi):
        compiler = tempfile.NamedTemporaryFile(suffix='.p', mode='wt+', delete=False)
        compiler.write('ROUTINE-LEVEL ON ERROR UNDO, THROW.\n\n')
        compiler.write('FUNCTION fCheckError RETURNS LOGICAL ():\n')
        compiler.write('   IF ERROR-STATUS:ERROR = NO\n')
        compiler.write('   THEN RETURN FALSE.\n\n')
        compiler.write('   DEFINE VARIABLE liError AS INTEGER   NO-UNDO.\n')
        compiler.write('   PUT UNFORMATTED SKIP.\n')
        compiler.write('   DO liError = 1 TO ERROR-STATUS:NUM-MESSAGES:\n')
        compiler.write('      PUT UNFORMATTED (IF COMPILER:WARNING THEN "Warning: " ELSE "ERROR: ") +\n')
        compiler.write('         ERROR-STATUS:GET-MESSAGE(liError) SKIP.\n')
        compiler.write('   END.\n\n')
        compiler.write('   RETURN TRUE.\n\n')
        compiler.write('END FUNCTION.\n')

        for ff in subfiles:
            if show == '.':
                compiler.write('PUT UNFORMATTED \'.\'.\n')
            elif show == 'name':
                compiler.write('PUT UNFORMATTED \'%s~n\'.\n' % ff)
            compiler.write(cline.replace('%s', ff) + '\n')
            compiler.write('fCheckError().\n')

        compiler.flush()
        compiler_p_list.append(compiler.name)

    return compiler_p_list

@target
def clean(*a):
    for file in glob('procore*') + glob(myself + '.pl'):
        os.unlink(file)

@target
def cui(*a):
    '''cui$|forcecui|vim$|vimbatch'''

    if a[0] == 'cui' and os.path.isfile('../maintenance'):
        print('Service break ongoing - aborting!')
        sys.exit(5)

    args = ['-pf', getpf('../db/progress/store/all')]

    if a[0] == 'cui' or a[0] == 'forcecui':
        program = 'Syst/tmslogin.p'
        args.extend(['-clientlog', '../var/log/tms_ui.log', '-logginglevel', '4'])
    else: # Only vim should use this block internally...
        if len(parameters) == 0:
            raise PikeException('Expected a module to run as a parameter')
        if a[0] == 'vimbatch':
            args.extend(['-b'])
        program = parameters[0]
        if os.path.isfile('{0}/progress.cfg.edit'.format(dlc)):
            os.environ['PROCFG'] = '{0}/progress.cfg.edit'.format(dlc)

    if 'current_dir' in globals():
        os.environ['PROPATH'] = current_dir + ',' + os.environ['PROPATH']

    cdr_dict = {}
    for cdr_database in cdr_databases:
        if not cdr_dict:
            cdr_dict = active_cdr_db_pf()
        if cdr_database in cdr_dict:
            args.extend(cdr_dict[cdr_database])

    args.extend(['-p', program])

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
    program = parameters[0]
    del parameters[0]

    dbcount = 0
    remove_from_parameters = []
    if any(x in parameters for x in ['all', 'all_except_cdr']):
        remove_from_parameters = databases + ['all', 'all_except_cdr']
        dbcount += len(databases)
        args.extend(['-pf', getpf('../db/progress/store/all')])
        if 'all' in parameters:
            remove_from_parameters.extend(cdr_databases)
            dbcount += len(cdr_databases)
            cdr_dict = active_cdr_db_pf()
            for db in cdr_dict:
                args.extend(cdr_dict[db])

    for pp in [item for item in parameters if item not in remove_from_parameters]:
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

    args.extend(['-p', program + '.p'])

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
    '''batch|mbatch'''

    if os.path.isfile('../maintenance'):
        print('Service break ongoing - aborting!')
        sys.exit(5)

    assert len(parameters) > 0, 'Which module to run?'
    batch_module = parameters[0]
    del parameters[0]
    
    module_base = os.path.basename(batch_module)
    if 'tenant' in globals():
        module_base = '{0}_{1}'.format(module_base,tenant)

    altdbs = []
    if 'alt' in globals():
        for altdb in alt.split(','):
            if os.path.isfile('{0}/db/progress/store/{1}_alt.pf'.format(relpath, altdb)):
                if len(altdb.split('@')) > 1:
                    if altdb.split('@')[1] == gethostname():
                        altdbs.append(altdb.split('@')[0])
                else:
                    altdbs.append(altdb)

    cdr_dict = {}

    if a[0] == 'batch':
       if os.path.exists('../var/run/%s.pid' % module_base):
           print('Lockfile %s.pid exists - aborting!' % module_base)
           sys.exit(5)
       fd = open('../var/run/%s.pid' % module_base, 'w')
       fd.write(str(os.getpid()))
       fd.close()

    args = ['-b', '-p', batch_module + '.p']

    dbcount = 0
    remove_from_parameters = []
    if any(x in parameters for x in ['all', 'all_except_cdr']):
        remove_from_parameters = databases + ['all', 'all_except_cdr']
        args.extend(['-pf', getpf('../db/progress/store/all')])
        dbcount += len(databases)
        if 'all' in parameters:
            remove_from_parameters.extend(cdr_databases)
            dbcount += len(cdr_databases)
            cdr_dict = active_cdr_db_pf()
            for db in cdr_dict:
                args.extend(cdr_dict[db])

    for pp in [item for item in parameters if item not in remove_from_parameters]:
        if pp in databases:
            args.extend(['-pf', getpf('../db/progress/store/{0}{1}'.format(pp, '_alt' if pp in altdbs else ''))])
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

    logfile = None
    try:
        if a[0] == 'batch':
            logfile = open('../var/log/%s.log' % module_base, 'a')
            if not skip_timelog:
                logfile.write(time.strftime('%F %T %Z') + ' {0}\n'.format('='*50))
                logfile.flush()
            cmd = Popen(mpro + args, stdout=logfile)
        else:
            cmd = Popen(mpro + args, stdout=PIPE)

        while cmd.poll() is None:
            try:
                cmd.wait()
                if a[0] != 'batch':
                    cmdoutput = cmd.stdout.read().rstrip('\n')
                    if cmdoutput:
                        print cmdoutput
            except KeyboardInterrupt:
                cmd.send_signal(2)
            else:
                sys.exit(cmd.returncode)
    finally:
        if a[0] == 'batch':
          os.unlink('../var/run/%s.pid' % module_base)
        if logfile is not None:
           logfile.close()

@target
def idbatch(*a):
    if os.path.isfile('../maintenance'):
        print('Service break ongoing - aborting!')
        sys.exit(5)

    assert len(parameters) > 0, 'Which module to run?'
    batch_module = parameters[0]
    del parameters[0]

    module_base = os.path.basename(batch_module)
    if 'tenant' in globals():
        module_base = '{0}_{1}'.format(module_base,tenant)

    cdr_dict = {}

    try:
        batchid = int(parameters[0])
        if batchid in databases:
            raise IndexError
    except ValueError:
        print('Batch ID must be a number - aborting!')
        sys.exit(5)
    except IndexError:
        print('No batch ID given - aborting!')
        sys.exit(5)

    del parameters[0]

    if os.path.exists('../var/run/%s_%s.pid' % (module_base, batchid)):
        print('Lockfile %s_%s.pid exists - aborting!' % (module_base, batchid))
        sys.exit(5)
    fd = open('../var/run/%s_%s.pid' % (module_base, batchid), 'w')
    fd.write(str(os.getpid()))
    fd.close()

    args = ['-b', '-p', batch_module + '.p']

    dbcount = 0
    remove_from_parameters = []
    if any(x in parameters for x in ['all', 'all_except_cdr']):
        remove_from_parameters = databases + ['all', 'all_except_cdr']
        args.extend(['-pf', getpf('../db/progress/store/all')])
        dbcount += len(databases)
        if 'all' in parameters:
            remove_from_parameters.extend(cdr_databases)
            dbcount += len(cdr_databases)
            cdr_dict = active_cdr_db_pf()
            for db in cdr_dict:
                args.extend(cdr_dict[db])

    for pp in [item for item in parameters if item not in remove_from_parameters]:
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

    logfile = None
    try:
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
            else:
                sys.exit(cmd.returncode)
    finally:
        os.unlink('../var/run/%s_%s.pid' % (module_base, batchid))
        if logfile is not None:
           logfile.close()

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

    if os.path.isfile('{0}/progress.cfg.edit'.format(dlc)):
        os.environ['PROCFG'] = '{0}/progress.cfg.edit'.format(dlc)

    args = parameters or (['-pf', getpf('../db/progress/store/all')])
    args = mpro + args + ['-clientlog', '../var/log/tms_editor.log', '-logginglevel', '4']
    os.execlp(args[0], *args)
