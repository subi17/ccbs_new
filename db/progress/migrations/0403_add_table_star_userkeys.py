from gearbox.migrations import Migration

class Adduserkeys(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('userkeys', area='Sta_Data_256')
        t.column('usercode', 'character', initial='',
                 label='User',
                 help='User code')
        t.column('keytype', 'character', format='X(20)', initial='',
                 label='Type',
                 help='Key type')
        t.column('key', 'character', format='X(30)', initial='',
                 label='Key',
                 help='Key value')
        t.index('keytype', ['keytype', 'key', 'usercode'], area='Sta_Index_1',
                unique=True)
        t.index('usercode', ['usercode', 'keytype', 'key'], area='Sta_Index_1',
                primary=True, unique=True)

    def down(self):
        self.drop_table('userkeys')

