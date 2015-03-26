from gearbox.migrations import Migration

class AddDialType(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('DialType', area='Sta_Data_256',
                       label='Dialling Type',
                       dump_name='dialtype',
                       desc='Dialling Type\
Dialling Type\
\
')
        t.column('DialType', 'integer', format='>>9', initial='0',
                 label='Dialling Type',
                 column_label='DT',
                 help='Dialling type code')
        t.column('DTName', 'character', format='x(30)', initial='',
                 label='Name',
                 help='Name for dialling type')
        t.index('DialType', ['DialType'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('DTName', ['DTName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('DialType')

