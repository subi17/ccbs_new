from gearbox.migrations import Migration

class AddMNPOperation(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MNPOperation', area='Sta_Data_64',
                       dump_name='mnpoper')
        t.column('MNPOperationID', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='MNPOperationID')
        t.column('CreatedTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='CreatedTS')
        t.column('StatusCode', 'integer', format='>>9', initial='0',
                 column_label='StatusCode')
        t.column('MNPSeq', 'integer', format='>>>>>>>9', initial='0',
                 column_label='MNPSeq')
        t.column('Sender', 'integer', format='>>9', initial='0',
                 column_label='Sender')
        t.column('SentTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 column_label='SentTS')
        t.column('MessageType', 'character', initial='',
                 column_label='MessageType')
        t.column('MsgTurn', 'integer', format='>>9', initial='0',
                 column_label='MsgTurn')
        t.column('XMLSeq', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='XMLSeq',
                 help='Reference to MNPXml table')
        t.column('XMLRequest', 'clob', format='x(40)',
                 label='XML Request', column_label='XMLRequest',
                 help='XML request message',
                 area='Lob_Data', size='1M')
        t.column('XMLResponse', 'clob', format='x(40)',
                 column_label='XML Response',
                 help='XML response message',
                 area='Lob_Data', size='1M')
        t.column('ErrorCode', 'character', format='x(10)', initial='',
                 column_label='ErrorCode')
        t.column('ErrorHandled', 'integer', format='>9', initial='0',
                 column_label='ErrorHandled')
        t.column('ErrorDesc', 'character', format='x(50)', initial='',
                 column_label='ErrorDesc',
                 help='Error description')
        t.index('CreatedTS', [('CreatedTS', 'DESCENDING')], area='Sta_Index_2')
        t.index('ErrorCode', ['ErrorHandled', 'ErrorCode'], area='Sta_Index_2')
        t.index('MNPOperation', ['MNPOperationID'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('MNPSeq', ['MNPSeq', 'CreatedTS'], area='Sta_Index_2')
        t.index('Sender', ['Sender', 'StatusCode'], area='Sta_Index_2')

    def down(self):
        self.drop_table('MNPOperation')

