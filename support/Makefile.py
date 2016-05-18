from pike import *
import os, sys
from subprocess import Popen, PIPE
import shlex

relpath = '..'
exec(open(relpath + '/etc/make_site.py').read())

def read_batchparameters(subdir, include_done_scripts):
    result = []
    for linenum, line in enumerate(open(subdir + '/batch_parameters', 'rt')):
        if not line.strip() or line.strip().startswith('#'):
            continue
        if not line.count(':') >= 2:
            print('Invalid syntax in line {0} of '
                  '{1}/batch_parameters'.format(linenum, subdir))
            continue
        script, title, params = line.strip().split(':', 2)
        if not os.path.exists(subdir+ '/' + script):
            print('Unknown script {0} in line {1} of'
                  ' {2}'.format(script, linenum, subdir))
            continue
        script = os.path.join(subdir, script)
        if title:
            logfile = script[:-2] + '_' + title + '.out'
            title = ' ({0})'.format(title)
        else:
            logfile = script[:-2] + '.out'
        if os.path.exists(logfile) and not include_done_scripts:
            continue
        result.append((script, title, shlex.split(params), logfile))
    return result

def expand_database_params(parameters, db_dir):
    for pp in parameters:
        if pp.startswith('@'):
            if ':' in pp:
                db, alias = pp[1:].split(':', 1)
            else:
                db, alias = pp[1:], None
            yield '-pf'
            yield db_dir + '/{0}.pf'.format(db)
            if alias is not None:
                yield '-ld'
                yield alias
        else:
            yield pp

def run_script(script, title, params, log):
    print('Running {0}{1}...'.format(script, title))
    outfile = open(log, 'wb')
    args = ['-T', '../var/tmp', '-b', '-p', script]
    args.extend(expand_database_params(params, '../db/progress/store'))
    pp = Popen(mpro + args, stdout=PIPE)
    for line in pp.stdout:
        outfile.write(line)
        sys.stdout.write(line.decode('latin-1'))
    outfile.close
    if pp.poll() != 0:
        os.rename(log, log[:-4] + '.err')
        print('** ERROR **')


@target
def default(*a):
    scripts = []
    for ii in os.listdir('.'):
        if ii == 'lib' or not os.path.isdir(ii) \
        or not os.path.exists(ii + '/batch_parameters'):
            continue
        scripts_in_this_version = read_batchparameters(ii, ii == appversion)
        if scripts_in_this_version:
            scripts.extend(scripts_in_this_version)

    input = ''
    while input.lower() not in ['q', 'quit', 'exit']:
        print('Select a script to run, or type q to quit:')
        for ii, (script, title, _p, log) in enumerate(scripts):
            if os.path.exists(log):
                print('  # {0}: {1}{2} done'.format(ii, script, title))
            else:
                print('  {0}: {1}{2}'.format(ii, script, title))
        input = sys.stdin.readline().strip()
        if input.isdigit() and int(input) < len(scripts):
            run_script(*scripts[int(input)])
        if input.lower() in ['a', 'all']:
            for cc in scripts:
                if not os.path.exists(cc[-1]):
                    run_script(*cc)

