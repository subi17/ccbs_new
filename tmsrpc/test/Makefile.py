from pike import *
from glob import glob
from subprocess import call, Popen, PIPE

class TestFailures(PikeException): pass

relpath = '../..'
exec(open(relpath + '/etc/make_site.py').read())

units = glob('unit/**/*.cls')
consistency_tests = glob('consistency/*.cls')
functionals = glob('functional/index.html')
scripts = glob('**/test_*.py')

@target
def test(*a):
    errors = 0
    for testtype in parameters or ['unit', 'functional', 'script', 'consistency']:
        try:
            require(testtype, [])
        except TestFailures:
            errors = 1
    if errors:
        raise TestFailures()

# Test called by build_and_test: skip concistency tests
@target
def ci_test(*a):
    require('test', ['unit', 'functional', 'script'])

@target(['db>ready'])
def unit(*a):
    def unit_test(pf, tests):
        test = Popen(mpro + ['-clientlog', '/dev/null', '-pf', 'db/' + pf,
                             '-param', ','.join(tests),
                             '-b', '-p', 'gearbox/unit/run_tests.r'],
                             stdout=PIPE)
        call('/bin/cat', stdin=test.stdout)
        if test.wait() != 0:
            raise TestFailures('Unit tests')
    unit_test('all.pf', (x for x in parameters or units))

@target
def consistency(*a):
    test = Popen(mpro + ['-clientlog', '/dev/null',
                        '-pf', '../../db/progress/store/all.pf',
                        '-param', ','.join(parameters or consistency_tests),
                        '-b', '-p', 'gearbox/unit/run_tests.r'], stdout=PIPE)
    call('/bin/cat', stdin=test.stdout)
    if test.wait() != 0:
        raise TestFailures('Consistency tests')

@target(['db>ready'])
def script(*a):
    for test in parameters or scripts:
        if call([test]) != 0:
            raise TestFailures(test)

@target(['db>ready'])
def functional(*a):
    errors = 0
    for test in parameters or functionals:
        errors += callgrep(mpro + ['-clientlog', '/dev/null',
                                   '-pf', 'db/all.pf',
                                  '-param', test + ',../../docs',
                                  '-b', '-p', 'gearbox/functional/run.r'], [])
    if errors != 0:
        raise TestFailures()

@target
def deps(*a):
    if len(parameters) != 1:
        raise PikeException('Expected source file as parameter')
    source_file = parameters[0]
    dependencies = Popen(mpro + ['-b', '-p', 'gearbox/unit/list_dependencies.p', '-param', source_file], stdout=PIPE)
    call('/bin/cat', stdin=dependencies.stdout)
