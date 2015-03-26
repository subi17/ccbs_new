from gearbox.migrations import Migration

class AddBSubRepl(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('BSubRepl', area='Sta_Data_256',
                       label='Replacement String For B-sub nos',
                       dump_name='bsrepl')
        t.column('BSubValue', 'character', initial='',
                 label='Br-Type',
                 column_label='Br-Type',
                 help='BSub Number Replacement Type')
        t.column('BSubRepl', 'character', format='x(16)', initial='',
                 label='Br-Key',
                 column_label='Br-Key',
                 help='B-Sub Number')
        t.column('BRName', 'character', format='x(30)', initial='',
                 label='Br-Name',
                 column_label='Br-Name',
                 help='Text Which Replaces The Number')
        t.column('Brand', 'character', initial='',
                 label='BrCode',
                 column_label='BrCode',
                 help='Code Of Brand')
        t.index('BRName', ['Brand', 'BSubValue', 'BSubRepl'], area='Sta_Index_3',
                primary=True)
        t.index('BSubValue', ['Brand', 'BSubValue', 'BSubRepl'], area='Sta_Index_3',
                unique=True)

    def down(self):
        self.drop_table('BSubRepl')

