from gearbox.migrations import Migration

class AddBBoard(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('BBoard', area='Sta_Data_256',
                       label='Bulletin Board Table',
                       dump_name='bboard',
                       desc='Bulletin Board Details')
        t.column('BBNumber', 'integer', format='>>>>>>>9', initial='0',
                 column_label='BBNumber',
                 help='Reference Number for Bulletin Board',
                 description='Auto-generated sequence number for Bulletin Board Message')
        t.column('TroTicket', 'integer', format='>,>>>,>>9', initial='0',
                 label='Reference',
                 column_label='Reference',
                 help='Reference no. for Customer',
                 description='Auto-generated sequence number for Trouble Ticket')
        t.column('CreStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='Date and time of Bulletin Board Message creation Time',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('Importance', 'character', initial='',
                 column_label='Importance',
                 help='Importance of Bulletin Board Message')
        t.column('Topic', 'character', format='x(60)', initial='',
                 column_label='Topic',
                 help='Bulletin Board Topic')
        t.column('BBText', 'character', format='x(60)', initial='',
                 label='Text',
                 column_label='Text',
                 help='Bulletin Board Message Text')
        t.column('ExpStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Expires',
                 column_label='Expires',
                 help='Date and time of Bulletin Board Message expiration',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('UserCode', 'character', initial='',
                 label='User',
                 help='Bulletin Board Message created By')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('bbnumber', ['Brand', 'BBNumber'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('importance', ['Brand', 'Importance'], area='Sta_Index_2')
        t.index('TroTicket', ['Brand', 'TroTicket', 'BBNumber'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('BBoard')

