from gearbox.migrations import Migration

class AddDepartment(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Department', area='Sta_Data_256',
                       label='Department',
                       dump_name='departme',
                       desc='One level of cost accounting\
\
\
')
        t.column('Department', 'character', initial='',
                 column_label='Department')
        t.column('DpName', 'character', format='x(30)', initial='',
                 label='Name',
                 help='Name of the department')
        t.column('Division', 'character', initial='',
                 column_label='Division',
                 help='Division of the department')
        t.index('Department', ['Department'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('DpName', ['DpName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('Department')

