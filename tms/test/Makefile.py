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
def ci_test(*a): pass

@target
def unit(*a): pass

@target
def consistency(*a): pass

@target
def script(*a): pass

@target
def functional(*a): pass

@target
def deps(*a): pass
