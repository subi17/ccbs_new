from gearbox.migrations import Migration

class AddTMSRepModel(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TMSRepModel', area='Sta_Data_256',
                       label='Report Models',
                       dump_name='tmsrepmo',
                       desc='Report models')
        t.column('RepNum', 'integer', format='>>9', initial='0',
                 label='Report nbr',
                 help='Report number')
        t.column('RepName', 'character', format='X(30)', initial='',
                 label='Report Name',
                 column_label='Name',
                 help='Name of Report')
        t.column('PrinterId', 'character', format='x(24)', initial='',
                 label='Logical Name',
                 column_label='Logical',
                 help='Logical name of the printer')
        t.column('Effect', 'character', format='x(1)', initial='')
        t.column('PageLength', 'integer', format='>>9', initial='0',
                 label='Lines per page',
                 column_label='Lines',
                 help='Lines per one page (max)')
        t.column('AvailLines', 'integer', format='>>9', initial='0',
                 label='Lines available',
                 column_label='Available',
                 help='Lines available on one page')
        t.column('UpdPerm', 'logical', initial='yes',
                 label='Show print options',
                 column_label='Show options',
                 help='Show the printing options for user when starting to print')
        t.index('RepNum', ['RepNum'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TMSRepModel')

