from gearbox.migrations import Migration

class AddMSISDNNUMBER(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('MSISDNNUMBER', area='Sta_Data_256',
                       dump_name='msisdnnu',
                       desc='A Single MSISDN number')
        t.column('CLI', 'character', format='X(11)', initial='',
                 label='MSISDN',
                 column_label='MSISDN No',
                 help='MSISDN Subscriber No')
        t.column('MsisdnType', 'integer', format='>9', initial='0',
                 label='MSISDNType',
                 column_label='MSISDNType',
                 help='MSISDN Type')
        t.column('Rank', 'integer', format='>>>9', initial='0',
                 column_label='MSISDNType')
        t.index('CLI', ['CLI'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('MSISDNNUMBER')

