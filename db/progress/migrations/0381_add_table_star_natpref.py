from gearbox.migrations import Migration

class AddNatPref(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('NatPref', area='Sta_Data_256',
                       label='NatPref',
                       dump_name='natpref',
                       desc='Special prefixes for national B-sub. numbers')
        t.column('BDest', 'character', format='x(16)', initial='',
                 label='Nat B-sub',
                 column_label='Nat B-sub',
                 help='A National B-subscriber number')
        t.column('Pref', 'character', initial='',
                 label='Prefix',
                 column_label='Prefix',
                 help='An alphanumeric prefix that is to be added into B-sub. no')
        t.column('Memo', 'character', format='x(30)', initial='',
                 column_label='Memo',
                 help='Memo Text')
        t.index('BDest', ['BDest'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('Pref', ['Pref', 'BDest'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('NatPref')

