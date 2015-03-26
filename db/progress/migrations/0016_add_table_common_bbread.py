from gearbox.migrations import Migration

class AddBBRead(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('BBRead', area='Sta_Data_256',
                       label='Bulletin Board Readers Table',
                       dump_name='bbread',
                       desc='Bulletin Board Readers Details')
        t.column('BBNumber', 'integer', format='>>>>>>>9', initial='0',
                 column_label='BBNumber',
                 help='Reference Number for Bulletin Board',
                 description='Auto-generated sequence number for Bulletin Board Message')
        t.column('UserCode', 'character', initial='',
                 label='Read By',
                 help='Bulletin Board User')
        t.column('ReadStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Read On',
                 column_label='Read On',
                 help='Date and time of Bulletin Board Message read Time',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('bbnumber', ['Brand', 'BBNumber', 'UserCode'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('BBRead')

