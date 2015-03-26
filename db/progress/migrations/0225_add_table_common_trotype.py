from gearbox.migrations import Migration

class AddTroType(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TroType', area='Sta_Data_256',
                       label='Trouble Type',
                       dump_name='trotype',
                       desc='Trouble Type')
        t.column('TTType', 'character', mandatory=True, initial='',
                 label='Type',
                 column_label='Type',
                 help='Code of trouble type')
        t.column('TTTName', 'character', format='x(30)', initial='',
                 label='Explanation',
                 column_label='Explanation',
                 description='Explanation')
        t.column('Handler', 'character', initial='',
                 label='Responsible Person',
                 column_label='Responsible Person',
                 help='Responsible person (handler) for this kind of troubles',
                 description='Responsible Person')
        t.column('Memo', 'character', initial='',
                 column_label='Memo')
        t.column('TTCat', 'character', mandatory=True, initial='',
                 label='Category',
                 column_label='Category',
                 help='Code of trouble category')
        t.column('Duration', 'decimal', format='>>9.99', initial='0.00',
                 column_label='Duration',
                 help='Default Type Duration')
        t.index('Handler', ['Handler'], area='Sta_Index_2')
        t.index('TTCat', ['TTCat', 'TTType'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TroType')

