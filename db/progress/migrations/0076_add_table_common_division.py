from gearbox.migrations import Migration

class AddDivision(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Division', area='Sta_Data_256',
                       label='Division',
                       dump_name='division',
                       desc='Uppermost level of cost accounting\
\
')
        t.column('Division', 'character', initial='',
                 column_label='Division')
        t.column('DvName', 'character', format='x(30)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of the division')
        t.index('Division', ['Division'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('DvName', ['DvName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('Division')

