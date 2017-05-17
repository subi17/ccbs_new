exec(open('../../../db/progress/store/Makefile.py').read())

db_running_msg = False

@target('create', 'start', 'migrate', *main_pf_files)
def ready(*a):
    pass
