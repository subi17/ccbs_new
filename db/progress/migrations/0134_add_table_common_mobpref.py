from gearbox.migrations import Migration

class AddMobPref(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MobPref', area='Sta_Data_256',
                       label='Mobile Prefixes',
                       dump_name='mobpref',
                       desc='Mobile prefixes')
        t.column('Prefix', 'character', initial='',
                 column_label='Prefix',
                 help='A prefix that identifies mobile numbers')
        t.column('Memo', 'character', format='x(30)', initial='',
                 label='memo',
                 column_label='memo',
                 help='Memo')
        t.column('Operator', 'character', mandatory=True, initial='',
                 label='OpCode',
                 column_label='OpCode',
                 help='Operators code')
        t.index('Prefix', ['Prefix'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('MobPref')

