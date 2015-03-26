from gearbox.migrations import Migration

class AddActionFlag(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ActionFlag', area='Sta_Data_256',
                       dump_name='actionfl')
        t.column('MsSeq', 'integer', format='>>>>>>>9', initial='0',
                 label='Subscription',
                 column_label='MsSeq',
                 help='Link to subscription')
        t.column('ActionType', 'integer', format='>9', initial='0',
                 label='Action Type',
                 column_label='Type',
                 help='Action type')
        t.column('Brand', 'character', initial='')
        t.column('TimeStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Time Stamp',
                 column_label='Time',
                 help='Time stamp')
        t.column('Memo', 'character', format='X(300)', initial='')
        t.index('ActionType', ['Brand', 'ActionType'], area='Sta_Index_2')
        t.index('Brand', ['Brand', ('TimeStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('MsSeq', ['MsSeq', 'ActionType', ('TimeStamp', 'DESCENDING')], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('ActionFlag')

