from gearbox.migrations import Migration

class AddTopUpQueue(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('TopUpQueue', area='Sta_Data_256',
                       dump_name='topupqueue')
        t.column('State', 'integer', format='>9', initial='0')
        t.column('PPRequest', 'int64', format='->,>>>,>>9', initial='0')
        t.column('CLI', 'character', initial='')
        t.column('TopUpAmt', 'decimal', decimals=2, initial='0')
        t.column('VatAmt', 'decimal', decimals=2, initial='0')
        t.column('Date', 'date')
        t.column('Source', 'character', initial='')
        t.index('State', ['State'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('TopUpQueue')

