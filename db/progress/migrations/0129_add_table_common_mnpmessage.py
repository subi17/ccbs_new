from gearbox.migrations import Migration

class AddMNPMessage(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MNPMessage', area='Sta_Data_64',
                       dump_name='mnpmessa')
        t.column('CreatedTS', 'decimal', decimals=5, format='99999999.99999', initial='0')
        t.column('XMLMessage', 'character', initial='')
        t.column('StatusCode', 'integer', initial='0')
        t.column('MNPSeq', 'integer', initial='0')
        t.column('Sender', 'integer', initial='0')
        t.column('SentTS', 'decimal', decimals=5, format='99999999.99999', initial='0')
        t.column('MessageType', 'character', initial='')
        t.column('MsgTurn', 'integer', format='>>9', initial='0')
        t.index('MNPSeq', ['MNPSeq'], area='Sta_Index_2',
                primary=True)
        t.index('Sender', ['Sender', 'StatusCode'], area='Sta_Index_2')

    def down(self):
        self.drop_table('MNPMessage')

