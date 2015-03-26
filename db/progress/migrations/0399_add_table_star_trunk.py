from gearbox.migrations import Migration

class AddTrunk(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('Trunk', area='Sta_Data_256',
                       label='Trunk',
                       dump_name='trunk',
                       desc='Trunk groups in each exchange')
        t.column('ExCode', 'character', initial='',
                 label='Switch',
                 column_label='Switch',
                 help='Swicth code')
        t.column('TrunkName', 'character', format='x(40)', initial='',
                 label='Trunk name',
                 column_label='Trunk name',
                 help='Name of a Circuit Group Route')
        t.column('TrunkCode', 'character', format='x(7)', initial='',
                 label='Trunk Code',
                 column_label='Trunk Code',
                 help='Trunk group\'s code')
        t.column('Memo', 'character', extent=5, format='x(60)', initial='',
                 column_label='Memo')
        t.column('OpCode', 'character', mandatory=True, initial='',
                 label='Operator',
                 column_label='Operator',
                 help='Operator code, 1 - 8 characters')
        t.column('TrInternat', 'logical', format='Int/Nat', initial='No',
                 label='Int.',
                 column_label='Int.',
                 help='International / National traffic')
        t.column('TrIn', 'logical', format='Out/In', initial='Yes',
                 label='Conn.',
                 column_label='Conn.',
                 help='In / Out traffic')
        t.column('TrunkGroup', 'character', format='x(16)', initial='',
                 label='Group',
                 column_label='Group',
                 help='GROUP Name')
        t.index('ExCode', ['ExCode', 'TrunkGroup', 'TrunkCode'], area='Sta_Index_2',
                primary=True)
        t.index('Operator', ['OpCode', 'ExCode', 'TrunkGroup', 'TrunkCode'], area='Sta_Index_2')
        t.index('TrunkCode', ['TrunkCode', 'ExCode'], area='Sta_Index_2')

    def down(self):
        self.drop_table('Trunk')

