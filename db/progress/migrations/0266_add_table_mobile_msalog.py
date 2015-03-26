from gearbox.migrations import Migration

class AddMsALog(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('MsALog', area='Sta_Data_256',
                       dump_name='msalog',
                       desc='\
\
\
')
        t.column('MsALog', 'integer', mandatory=True, format='>>>>>>9', initial='0',
                 label='Sa-seq',
                 column_label='Sa-seq',
                 help='Sequence of Send Activation LOG')
        t.column('FirstName', 'character', format='x(20)', initial='',
                 label='Filename',
                 column_label='Filename')
        t.column('SaAts', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Activated',
                 column_label='Activated',
                 help='Activate time')
        t.column('SaSts', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Sent',
                 column_label='Sent',
                 help='Sending time')
        t.column('SaRts', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Received',
                 column_label='Received',
                 help='When File received from Operator')
        t.column('Memo', 'character', extent=15, format='x(70)', initial='',
                 column_label='Memo',
                 help='Memo text')
        t.column('SaStatus', 'integer', format='9', initial='0',
                 label='Status',
                 column_label='Status',
                 help='Status of File')
        t.column('SaQty', 'integer', extent=4, format='>>>>9', initial='0',
                 label='Amount',
                 column_label='Amount',
                 help='Amount of Mobile Subscription')
        t.index('MsALog', ['MsALog'], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('SaStatus', ['SaStatus', 'SaAts', 'MsALog'], area='Sta_Index_3',
                unique=True)

    def down(self):
        self.drop_table('MsALog')

