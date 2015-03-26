from gearbox.migrations import Migration

class AddCDRAmt(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('CDRAmt', area='Sta_Data_256',
                       label='CDRAmt',
                       dump_name='cdramt',
                       desc='Summary of read CDRs for OnLine displaying')
        t.column('Date', 'date', format='99-99-99',
                 column_label='Date',
                 help='Date of the counter')
        t.column('ExCode', 'character', initial='',
                 label='Switch',
                 column_label='Switch',
                 help='Calls from switch')
        t.column('QtyTot', 'integer', extent=24, format='>,>>>,>>9', initial='0',
                 label='Amt',
                 column_label='Amt',
                 help='Amount of calls per hour')
        t.column('QtyDB', 'integer', extent=24, format='>,>>>,>>9', initial='0',
                 label='#DB',
                 column_label='#DB',
                 help='Amount of calls per hour saved in the database')
        t.column('QtyJunk', 'integer', extent=24, format='>,>>>,>>9', initial='0',
                 label='#Junk',
                 column_label='#Junk',
                 help='Amount of calls per hour dumped into junk')
        t.index('ExCode', ['ExCode', 'Date'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('CDRAmt')

