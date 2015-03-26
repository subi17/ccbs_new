from gearbox.migrations import Migration

class AddMobCount(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('MobCount', area='Sta_Data_128',
                       dump_name='mobcount')
        t.column('MsSeq', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='SubSeq',
                 column_label='SubSeq',
                 help='Sequence for a Subscription')
        t.column('CountType', 'character', initial='',
                 label='MC-Type',
                 column_label='MC-Type',
                 help='Mobile Counter Type')
        t.column('Balance', 'decimal', decimals=2, initial='0',
                 column_label='Balance',
                 help='Current unbilled balance')
        t.column('QtyLimit', 'integer', initial='0',
                 label='Limit',
                 column_label='Limit',
                 help='Mobile Counter alarm limit')
        t.column('CallQty', 'integer', format='>>>>>>9', initial='0',
                 label='Count',
                 column_label='Count',
                 help='Amount of unbilled calls')
        t.column('Duration', 'integer', format='>>>>>>>>9', initial='0',
                 label='Sec.',
                 column_label='Sec.',
                 help='Seconds of unbilled calls')
        t.column('CreditLimit', 'integer', initial='0',
                 label='Call Limit',
                 column_label='Call Limit',
                 help='Mobile Counter alarm limit')
        t.index('MsSeq', ['MsSeq', 'CountType'], area='Sta_Index_3',
                primary=True, unique=True)

    def down(self):
        self.drop_table('MobCount')

