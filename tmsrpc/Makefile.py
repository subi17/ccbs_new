from pike import *
import os
import shutil
import json
import tempfile
import fnmatch
import errno
import getpass
import xmlrpclib
import threading
from ast import literal_eval
from subprocess import call, Popen, PIPE
from socket import gethostname, error
from string import Template

relpath = '..'
exec(open(relpath + '/etc/make_site.py').read())
exec(open(relpath + '/tools/fcgi_agent/xmlrpc/help.py').read())
exec(open('getpf.py').read())

pike = which('pike')
if pike == None:
    raise PikeException("Cannot figure out pike location")

if environment == 'slavedevelopment':
    configfile = 'slavedevelopment_config.json'
elif environment == 'development' and not os.path.exists(gethostname() + '_config.json'):
    configfile = 'development_config.json'
else:
    configfile = gethostname() + '_config.json'

if not os.path.exists(configfile):
    raise PikeException('Failed, no lighttpd configuration file ' + configfile + ' found')	

show_file = False

state_base = os.path.abspath(os.path.join('..', 'var', 'run')) + '/'

lighttpd_template = Template('''server.document-root = "/var/tmp"
server.errorlog = "${work_dir}/var/log/lighttpd_${rpc}_${port}_error.log"
accesslog.filename = "${work_dir}/var/log/lighttpd_${rpc}_${port}_access.log"
server.pid-file = "${work_dir}/var/run/lighttpd_${rpc}_${port}.pid"
server.port = $port
''')

fastcgi_template = Template('''    ("bin-path" => "${pike} -C ${work_dir}/tmsrpc/${fcgi} run_agent ${fcgi}",
     "socket" => "${work_dir}/var/run/${fcgi}-${port}.socket",
''')

def recursive_overwrite(src, dest, ignore=None):
    if os.path.isdir(src):
        if not os.path.isdir(dest):
            os.makedirs(dest)
        files = os.listdir(src)
        if ignore is not None:
            ignored = ignore(src, files)
        else:
            ignored = set()
        for f in files:
            if f not in ignored:
                recursive_overwrite(os.path.join(src, f),
                                    os.path.join(dest, f),
                                    ignore)
    else:
        shutil.copyfile(src, dest)

def userandpass():
    if 'tenancies' in globals():
        for t, tdict in tenancies.items():
            if tdict['tenanttype'] == 'Super' or len(tenancies) == 1:
                return ['-U', '{0}@{1}'.format(tdict['username'], tdict['domain']), '-P', tdict['password'] ]
        raise ValueError('Cannot identify a super tenant')
    else:
        return []

def active_cdr_db_pf():
    if '-S' in open('../db/progress/store/common.pf').read():
        connection_type = "tcp"
    else:
        connection_type = "local"

    args = ['-b', '-p', 'Syst/list_active_cdr_databases.p', '-param', connection_type]
    args.extend(['-pf', getpf('../db/progress/store/common'), '-h', '1'])

    cdr_fetch = Popen(mpro + args, stdout=PIPE)
    dict = literal_eval(cdr_fetch.communicate()[0])

    if 'tenancies' in globals():
        uandp = userandpass()
        for db in dict:
            dict[db].extend(uandp)

    return dict

def mkdir_p(directory):
    try:
        os.makedirs(directory)
    except OSError as exc:  # Python >2.5
        if exc.errno == errno.EEXIST and os.path.isdir(directory):
            pass
        else:
            raise

def param_gen(a):
   for item in a:
       yield item.partition(':')

def do_lighttpd_conf(a):
    rpcdict = {}
    for item in [param for param in param_gen(a)]:
        if item[0] in rpcdict:
            rpcdict[item[0]].append(item[2])
        else:
            rpcdict[item[0]] = [item[2]]

    with open(configfile, 'r') as jsonfile:
        jsondata = json.load(jsonfile)
        for rpc in jsondata:
            if not rpcdict or rpc in rpcdict:
                for port in jsondata[rpc]:
                    if not rpcdict or '' in rpcdict[rpc] or port in rpcdict[rpc]:
                        lighttpdparams = (param for param in jsondata[rpc][port] if not param.startswith('/') if param != "conditional_param")
                        fcgis = (param for param in jsondata[rpc][port] if param.startswith('/'))

                        with open(state_base + 'lighttpd_' + rpc + '_' + port + '.conf', 'wt') as fd:

                            fd.write('# Auto-generated lighttpd-conf\n\n')

                            for lighttpdparam in lighttpdparams:
                                fd.write('{0} = {1}\n'.format(lighttpdparam, jsondata[rpc][port][lighttpdparam]))

                            fd.write(lighttpd_template.substitute(work_dir=work_dir, rpc=rpc, port=port))

                            fd.write("\nfastcgi.server = (\n")

                            fcgi_end = ''
                            for fcgi in fcgis:
                                fd.write(fcgi_end + '  "{0}" => (\n'.format(fcgi) )
                                fd.write(fastcgi_template.substitute(pike=pike, work_dir=work_dir, fcgi=jsondata[rpc][port][fcgi][0], port=port))

                                fcgiparam_end = ''
                                for fcgiparam, fcgivalue in jsondata[rpc][port][fcgi][1].items():
                                   fd.write(fcgiparam_end + '     "{0}" => {1}'.format(fcgiparam, fcgivalue))
                                   fcgiparam_end = ',\n'

                                if not fcgiparam_end == '':
                                   fd.write('),\n')

                                fcgi_end = '  ),\n'

                            if not fcgi_end == '':
                                fd.write('  )\n')

                            fd.write(')\n')

                            conditional_param = jsondata[rpc][port].get('conditional_param')

                            if conditional_param:
                                for condvalue, condparamlist in conditional_param.items():
                                    fd.write('\n{0} {{\n'.format(condvalue))
                                    for condparam in condparamlist:
                                        fd.write('    {0} = {1}\n'.format(condparam['param'],condparam['value']))
                                    fd.write('}\n')

                            fd.close()
        jsonfile.close()

def lighttpd_gen(a):
    rpcdict = {}
    for item in [param for param in param_gen(a)]:
        if item[0] in rpcdict:
            rpcdict[item[0]].append(item[2])
        else:
            rpcdict[item[0]] = [item[2]]

    with open(configfile, 'r') as jsonfile:
        jsondata = json.load(jsonfile)
        for rpc in jsondata:
            if not rpcdict or rpc in rpcdict:
                for port in jsondata[rpc]:
                    if not rpcdict or '' in rpcdict[rpc] or port in rpcdict[rpc]:
                        yield 'lighttpd_{0}_{1}'.format(rpc, port)
        jsonfile.close()

@target('test>test')
def test(*a): pass

@target
def compile(match, *a):
    '''compile$|compilec$|preprocess$|xref$'''

    if parameters:
        for rpc in parameters:
            if rpc not in rpcs:
                raise PikeException('Invalid rpc name')

    systemrpc_compiledir = ''

    rpc_having_source = _compile(match, rpclist=list(set(parameters or rpcs)))

    _compile_cls(match, rpclist=list(set(parameters or rpcs)))

    if match == 'compile' and rpc_having_source:
        # Compile systemrpc to temporary directory
        systemrpc_compiledir = tempfile.mkdtemp()
        _compile(match, work_dir + '/tools/fcgi_agent/systemrpc', systemrpc_compiledir, [''])
        for rpc in rpc_having_source:
            recursive_overwrite(systemrpc_compiledir, '{0}/tmsrpc/{1}/rpcmethods'.format(work_dir, rpc))
        shutil.rmtree(systemrpc_compiledir)

class myThread (threading.Thread):
   def __init__(self, compfile, files, args, compdir):
      threading.Thread.__init__(self)
      self.compfile = compfile
      self.files = files
      self.args = args
      self.compdir = compdir
   def run(self):
      worker(self.compfile, self.files, self.args, self.compdir)

def worker(compfile, files, args, compdir):
    """thread worker function"""
    comp = Popen(mpro + args + ['-b', '-inp', '200000', '-tok', '20000', '-p', compfile], stdout=PIPE)
    call('/bin/cat', stdin=comp.stdout)
    comp.wait()
    os.unlink(compfile)

    if compdir:
        helper_dir = work_dir + '/tools/fcgi_agent/xmlrpc/'
        for rpc, reldir, sourcefile in files:
            if rpc:
                file = '{0}/pp{1}/{2}'.format(rpc,reldir,sourcefile)
            else:
                file = '{0}/pp{1}/{2}'.format(compdir,reldir,sourcefile)
            sigandhelpfile = '{0}/doc{1}/{2}'.format(rpc + '/rpcmethods' if rpc else compdir, reldir, sourcefile)
            if not sourcefile == 'system__multicall.p':
                fd = open(sigandhelpfile.replace('.p','.sig'), 'wb')
                call([sys.executable, helper_dir + 'signature.py', file], stdout=fd)
                fd.close()

            helpobj = DocFile(file)
            with open(sigandhelpfile.replace('.p', '.help'), 'wb') as fd:
                fd.write(helpobj.to_helpfile())

            with open(sigandhelpfile.replace('.p', '.confluence'), 'wb') as fd:
                fd.write(helpobj.to_confluence())

class myThreadOnlyCompile (threading.Thread):
   def __init__(self, compfile, args):
      threading.Thread.__init__(self)
      self.compfile = compfile
      self.args = args
   def run(self):
      worker_only_compile(self.compfile, self.args)

def worker_only_compile(compfile, args):
    """thread worker function"""
    comp = Popen(mpro + args + ['-b', '-inp', '200000', '-tok', '20000', '-p', compfile], stdout=PIPE)
    call('/bin/cat', stdin=comp.stdout)
    comp.wait()
    os.unlink(compfile)

def _compile(compile_type, source_dir='', compile_dir='', rpclist=None):

    if not 'multi' in globals():
        global multi
        multi = 1
    else:
        try:
           multi = int(multi)
        except ValueError as verr:
          raise PikeException('multi must be integer from 1 to 16')
        except Exception as ex:
          raise PikeException('multi must be integer from 1 to 16')

        if int(multi) > 16 or int(multi) < 1:
            raise PikeException('multi must be integer from 1 to 16')

    source_files = []
    rpc_having_source = []

    if not source_dir:
        source_dir = '{0}/src'

    # For the compile command there are following position parameters
    # (the same information is stored to source_files variable tuple values)
    # 0 = <rpc>
    # 1 = relative_dir (if available begins with flash character)
    # 2 = source_file_name (no path)

    if compile_type == 'compile':
        compile_dir = compile_dir if compile_dir else '{0}/rpcmethods'
        if rpclist[0] != '':
            compilecommand = 'COMPILE {0}/src{1}/{2} PREPROCESS {0}/pp{1}/{2} NO-ERROR.\nCOMPILE {3}/tmsrpc/{0}/pp{1}/{2} SAVE INTO {0}/rpcmethods{1} NO-ERROR.'.format('{0}','{1}','{2}',work_dir)
        else:
            compilecommand = 'COMPILE {3}{0}{1}/{2} PREPROCESS {4}/pp{1}/{2} NO-ERROR.\nCOMPILE {4}/pp{1}/{2} SAVE INTO {4}{1} NO-ERROR.'.format('{0}','{1}','{2}',source_dir,compile_dir)
    elif compile_type == 'compilec':
        compile_dir = ''
        compilecommand = 'COMPILE {0}/src{1}/{2} NO-ERROR.'
    elif compile_type == 'preprocess':
        compile_dir = '{0}/pp'
        compilecommand = 'COMPILE {0}/src{1}/{2} PREPROCESS {0}/pp{1}/{2} NO-ERROR.'
    elif compile_type == 'xref':
        compile_dir = '{0}/xref'
        compilecommand = 'COMPILE {0}/src{1}/{2} XREF {0}/xref{1}/{2} NO-ERROR.'

    for rpc in rpclist:
        source_dir_to_use = source_dir.format(rpc)
        if not os.path.isdir(source_dir_to_use):
            continue
        compile_dir_to_use = compile_dir.format(rpc) if compile_dir else ''
        seen = []
        for root, dirs, files in os.walk(source_dir_to_use):
            for filename in fnmatch.filter(files, '*.p') + fnmatch.filter(files, '*.cls'):
                relative_dir = root.replace(source_dir_to_use,'')
                if relative_dir.startswith('/'):
                    relative_dir = relative_dir[1:]
                source_files.append(tuple([rpc, '/' + relative_dir if relative_dir else relative_dir, filename]))
                if compile_dir:
                    if relative_dir not in seen:
                        mkdir_p(os.path.join(compile_dir_to_use, relative_dir))
                        if compile_type == 'compile':
                            mkdir_p(os.path.join(os.path.join('{0}/doc'.format(compile_dir_to_use), relative_dir)))
                            if rpc:
                                mkdir_p(os.path.join('{0}/pp'.format(rpc), relative_dir))
                            else:
                                mkdir_p(os.path.join('{0}/pp'.format(compile_dir), relative_dir))
                        seen.append(relative_dir)
                    if rpc not in rpc_having_source:
                        rpc_having_source.append(rpc)

    if not rpc_having_source:
        return rpc_having_source

    args = ['-pf', getpf('../db/progress/store/all')]
    dbcount = len(databases)

    cdr_dict = {}
    for cdr_database in cdr_databases:
        if not cdr_dict:
            cdr_dict = active_cdr_db_pf()
        if cdr_database in cdr_dict:
            args.extend(cdr_dict[cdr_database])
            dbcount += 1

    if environment == 'safeproduction':
        os.environ['PROPATH'] = os.environ['PROPATH'].split(',', 1)[1]

    if os.path.isfile('{0}/progress.cfg.edit'.format(dlc)):
        os.environ['PROCFG'] = '{0}/progress.cfg.edit'.format(dlc)

    args.extend(['-h', str(dbcount)])

    threads = []
    for subfiles in chunks(source_files, (len(source_files) + multi - 1) // multi):
        compile_p = make_compiler(compilecommand, subfiles, show='name' if show_file else '.')
        thread = myThread(compile_p, subfiles, args, compile_dir if compile_type == 'compile' else '')
        thread.start()
        threads.append(thread)

    # Wait for all threads to complete
    for t in threads:
        t.join()

    if compile_type == 'compile' and rpclist[0] == '':
        shutil.rmtree('{0}/pp'.format(compile_dir))

    print('')

    return rpc_having_source

def _compile_cls(compile_type, rpclist):

    if not 'multi' in globals():
        global multi
        multi = 1
    else:
        try:
           multi = int(multi)
        except ValueError as verr:
          raise PikeException('multi must be integer from 1 to 16')
        except Exception as ex:
          raise PikeException('multi must be integer from 1 to 16')

        if int(multi) > 16 or int(multi) < 1:
            raise PikeException('multi must be integer from 1 to 16')

    source_files = []

    source_dir = '{0}/cls'

    # For the compile command there are following position parameters
    # (the same information is stored to source_files variable tuple values)
    # 0 = <rpc>
    # 1 = relative_dir (if available begins with flash character)
    # 2 = source_file_name (no path)

    if compile_type == 'compile':
        compile_dir = '{0}/rpcmethods'
        compilecommand = 'COMPILE {0}/cls{1}/{2} SAVE INTO {0}/rpcmethods NO-ERROR.'
    elif compile_type == 'compilec':
        compile_dir = ''
        compilecommand = 'COMPILE {0}/cls{1}/{2} NO-ERROR.'
    elif compile_type == 'preprocess':
        compile_dir = '{0}/pp'
        compilecommand = 'COMPILE {0}/cls{1}/{2} PREPROCESS {0}/pp{1}/{2} NO-ERROR.'
    elif compile_type == 'xref':
        compile_dir = '{0}/xref'
        compilecommand = 'COMPILE {0}/cls{1}/{2} XREF {0}/xref{1}/{2} NO-ERROR.'

    for rpc in rpclist:
        source_dir_to_use = source_dir.format(rpc)
        if not os.path.isdir(source_dir_to_use):
            continue
        compile_dir_to_use = compile_dir.format(rpc) if compile_dir else ''
        if compile_dir_to_use:
            mkdir_p(compile_dir_to_use)
        for root, dirs, files in os.walk(source_dir_to_use):
            for filename in fnmatch.filter(files, '*.cls'):
                relative_dir = root.replace(source_dir_to_use,'')
                if relative_dir.startswith('/'):
                    relative_dir = relative_dir[1:]
                source_files.append(tuple([rpc, '/' + relative_dir if relative_dir else relative_dir, filename]))

    if not source_files:
        return

    args = ['-pf', getpf('../db/progress/store/all')]
    dbcount = len(databases)

    cdr_dict = {}
    for cdr_database in cdr_databases:
        if not cdr_dict:
            cdr_dict = active_cdr_db_pf()
        if cdr_database in cdr_dict:
            args.extend(cdr_dict[cdr_database])
            dbcount += 1

    if environment == 'safeproduction':
        os.environ['PROPATH'] = os.environ['PROPATH'].split(',', 1)[1]

    if os.path.isfile('{0}/progress.cfg.edit'.format(dlc)):
        os.environ['PROCFG'] = '{0}/progress.cfg.edit'.format(dlc)

    args.extend(['-h', str(dbcount)])

    threads = []
    for subfiles in chunks(source_files, (len(source_files) + multi - 1) // multi):
        compile_p = make_compiler(compilecommand, subfiles, show='name' if show_file else '.')
        thread = myThreadOnlyCompile(compile_p, args)
        thread.start()
        threads.append(thread)

    # Wait for all threads to complete
    for t in threads:
        t.join()

    print('')

def chunks(l, n):
    """Yield successive n-sized chunks from l."""
    for i in xrange(0, len(l), n):
        yield l[i:i + n]

def make_compiler(cline, files, show='.'):
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

    for rpc, reldir, sourcefile in files:
        if show == '.':
            compiler.write('PUT UNFORMATTED \'.\'.\n')
        elif show == 'name':
            compiler.write('PUT UNFORMATTED \'{0}~n\'.\n'.format(cline.format(rpc,reldir,sourcefile)))
        compiler.write(cline.format(rpc,reldir,sourcefile) + '\n')
        compiler.write('fCheckError().\n')

    compiler.flush()

    return compiler.name

@target
def start(*a):
    '''start|run|rundaemons'''
    do_lighttpd_conf(parameters)

    for lighttpd in lighttpd_gen(parameters):
        pidfile = state_base + lighttpd + '.pid'
        if os.path.exists(pidfile):
            print('No need to start already running daemon ' + lighttpd)
        else:
            call([lighttpd_location, '-f', state_base + lighttpd + '.conf'])
            if not os.path.exists(pidfile):
                raise PikeException('Failed, no pidfile ' + pidfile + ' found')
            print('STARTED daemon ' + lighttpd)

@target
def status(*a):
    '''status|daemonsstatus|daemonstatus'''
    for lighttpd in lighttpd_gen(parameters):
        pidfile = state_base + lighttpd + '.pid'
        if not os.path.exists(pidfile):
            print('Daemon ' + lighttpd + ' is not running')
        else:
            fd = open(pidfile, 'rt')
            pid = fd.read().strip()
            fd.close()
            print('Daemon ' + lighttpd + ' is running with pid %s' % pid)
            
@target
def stop(*a):
    '''stop|stopdaemons'''
    for lighttpd in lighttpd_gen(parameters):
        pidfile = state_base + lighttpd + '.pid'
        if os.path.exists(pidfile):
            fd = open(pidfile, 'rt')
            pid = fd.read().strip()
            fd.close()
            call(['kill', '-INT', pid])
            print('Kill signal sent to daemon ' + lighttpd + ' pid %s' % pid)

@target
def build(*a):
    '''build|buildextapi'''
    if len(parameters) != 1:
        raise PikeException('Expected build_dir as parameter')
    build_dir = parameters[0]

    if build_dir[0] != "/":
        build_dir = os.getcwd() + '/' + build_dir

    if not os.path.exists(build_dir):
        os.mkdir(build_dir)

    for file in ['Makefile.py', 'getpf.py'] + glob('*_config.json'):
        shutil.copy(file, build_dir)

    if a[0] == 'buildextapi':
        print('Using r-files located on rpcmethods directories.')
        print('Please make sure that you have executed a command "pike compile" on a tmsrpc directory')
    else:
        require('compile', [])


    currentdir = os.getcwd()
    for rpc in rpcs:
        os.chdir(rpc)
        rpcbuilddir = os.path.join(build_dir, rpc)
        mkdir_p(rpcbuilddir)
        shutil.copy2('Makefile.py', rpcbuilddir)

        if os.path.exists('rpcmethods.pl'):
            os.unlink('/rpcmethods.pl')
        call([dlc + '/bin/prolib', 'rpcmethods.pl', '-create'])
        for dir, _dirs, files in os.walk('rpcmethods'):
            for file in files:
                if file.endswith('.r') \
                or file.endswith('.help') \
                or file.endswith('.sig'):
                    call([dlc + '/bin/prolib', 'rpcmethods.pl', '-add',
                          os.path.join(dir, file)])
        shutil.move('rpcmethods.pl', rpcbuilddir)
        os.chdir(currentdir)

def connect_confluence(host):

    # Python 3 doesn't have raw_input but it has input
    try:
        input = raw_input
    except NameError:
        pass

    while True:
        print('Connecting to host {}'.format(host))
        print('Please enter username and password (leave username empty to skip this host)')

        username = input('Username: ')

        if username == '':
            return False, None, None

        password = getpass.getpass('Password: ')

        server = xmlrpclib.ServerProxy(host + '/rpc/xmlrpc')

        try:
            session = server.confluence2.login(username, password)
            break
        except xmlrpclib.Fault as err:
            print "A fault occurred"
            print "Fault code: %d" % err.faultCode
            print "Fault string: %s" % err.faultString
        except xmlrpclib.ProtocolError as err:
            print "A protocol error occurred"
            print "URL: %s" % err.url
            print "HTTP/HTTPS headers: %s" % err.headers
            print "Error code: %d" % err.errcode
            print "Error message: %s" % err.errmsg
        except error:
            # Not connected ; socket error mean that the service is unreachable.
            print "Cannot connect to host: %s" % host
            return False, None, None

    return True, server, session

@target
def documentation(*a):
    if parameters:
        for rpc in parameters:
            if rpc not in rpcs:
                raise PikeException('Invalid rpc name')

    confluencefile = 'confluence.json'

    if not os.path.exists(confluencefile):
        raise PikeException("Cannot find {0} file".format(confluencefile))

    print('NOTE: Before running this command please make sure that you')
    print('      have updated the documentation files using')
    print('      the "pike compile" command in this directory!\n')

    rpclist = list(set(parameters or rpcs))

    with open(confluencefile, 'r') as jsonfile:
        jsondata = json.load(jsonfile)
        for host in jsondata:

            hostandparamrpc = [item for item in jsondata[host] if item in rpclist]

            if not hostandparamrpc:
                continue

            connected, server, session = connect_confluence(host)

            if not connected:
                continue

            for rpc in [item for item in jsondata[host] if item in rpclist]:
                docdir = '{0}/rpcmethods/doc'.format(rpc)

                space = ''
                title = ''
                try:
                    space = jsondata[host][rpc]['Space']
                    title = jsondata[host][rpc]['Title']
                except KeyError, e:
                    raise PikeException("{0}: Missing Space and/or Title element. host={1}, rpc={2}".format(confluencefile, host, rpc))

                print('Trying to access space {0} having title {1}'.format(space, title))

                try:
                    page = server.confluence2.getPage(session, space, title)
                    print('The access was successful')
                except xmlrpclib.Fault, err:
                    print "A fault occurred"
                    print "Fault code: %d" % err.faultCode
                    print "Fault string: %s" % err.faultString
                    print "--"
                    continue

                newtext = ''
                data = []

                for root, dirs, files in os.walk(docdir):
                    for filename in fnmatch.filter(files, '*.confluence'):
                        with open(os.path.join(root, filename), "r") as myfile:
                            data.append(myfile.read())
                            myfile.close()

                newtext = '\n'.join(data)

                # It is not possible to do the comparison as confluence2.getPage
                # confluence2.getPage returns data in xml format and the newtext
                # data is Wiki formatted.
                # TODO: Implement xml format support to help.py
                #       i.e. new version of to_confluence() method.

                #if page['content'] != newtext:
                #    page['content'] = newtext
                #    server.confluence2.storePage(session, page)
                #    print "The page has been updated"
                #else:
                #    print "The page was already up to date"

                page['content'] = newtext
                server.confluence1.storePage(session, page)
                print "The page has been updated"

                print('--')

            server.confluence2.logout(session)

        jsonfile.close()
