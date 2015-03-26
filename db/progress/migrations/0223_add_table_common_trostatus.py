from gearbox.migrations import Migration

class AddTroStatus(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TroStatus', area='Sta_Data_256',
                       label='Trouble Status',
                       dump_name='trostatu',
                       desc='Trouble Status')
        t.column('TTStatus', 'character', mandatory=True, initial='',
                 label='Status',
                 column_label='Status',
                 help='Code of trouble status')
        t.column('Memo', 'character', format='x(30)', initial='',
                 label='Explanation',
                 column_label='Explanation',
                 help='Explanation(description) of trouble status',
                 description='Explanation')
        t.index('TTStatus', ['TTStatus'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TroStatus')

