from gearbox.migrations import Migration

class Addsearchfields(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('searchfields', area='Sta_Data_256',
                       label='Search Fields',
                       dump_name='searchfi',
                       desc='Automatic search fields')
        t.column('fieldname', 'character', mandatory=True, format='X(15)', initial='',
                 label='Field',
                 help='Field name of search value')
        t.column('tablename', 'character', format='X(15)', initial='',
                 label='Table',
                 help='Table name if needed')
        t.column('program', 'character', format='X(50)', initial='',
                 label='Program',
                 help='Program with recursize directory, if needed')
        t.column('sdo', 'character', mandatory=True, format='X(40)', initial='',
                 label='SDO',
                 help='Name of SDO. Recuisive directory name')
        t.column('appsrv', 'character', mandatory=True, format='X(20)', initial='',
                 label='AppServer',
                 help='Logical name of Application Server')
        t.column('searchprogram', 'character', mandatory=True, format='X(50)', initial='lib/wsearch.w',
                 label='Search program',
                 help='Search program name')
        t.column('valuefield', 'character', mandatory=True, format='X(20)', initial='',
                 label='Value field',
                 help='Field name of returned value')
        t.column('displayfields', 'character', mandatory=True, format='X(256)', initial='',
                 label='Display fields',
                 help='Field\'s what it shown to search browser')
        t.column('viewerprogram', 'character', format='X(50)', initial='',
                 label='Viewer',
                 help='Viewer program, if you needed')
        t.index('fieldname', ['fieldname', 'tablename', 'program'], area='Sta_Index_2',
                primary=True)
        t.index('program', ['program', 'tablename', 'fieldname'], area='Sta_Index_2')
        t.index('sdo', ['sdo', 'tablename', 'fieldname'], area='Sta_Index_2')
        t.index('tablename', ['tablename', 'fieldname', 'program'], area='Sta_Index_2')

    def down(self):
        self.drop_table('searchfields')

