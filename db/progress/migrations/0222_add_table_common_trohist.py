from gearbox.migrations import Migration

class AddTroHist(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TroHist', area='Sta_Data_256',
                       label='Trouble Ticket History',
                       dump_name='trohist',
                       desc='Trouble Ticket history')
        t.column('TTNum', 'integer', format='>,>>>,>>9', initial='0',
                 label='Reference',
                 column_label='Reference',
                 help='Reference no. for Customer',
                 description='Auto-generated sequence number for Trouble Ticket')
        t.column('CreStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='Date and time of trohist creation',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('Event', 'character', format='x(60)', initial='',
                 column_label='Event',
                 help='History Event')
        t.column('CreUser', 'character', initial='',
                 label='Created By',
                 help='User code')
        t.index('TTNum', ['TTNum', 'CreStamp'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('TroHist')

