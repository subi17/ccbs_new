from gearbox.migrations import Migration

class AddCallAlarm(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('CallAlarm', area='Sta_Data_64',
                       dump_name='callalar')
        t.column('CASeq', 'integer', format='>>>>>>>9', initial='0',
                 label='Seq',
                 column_label='Seq',
                 help='Call Alarm Sequence')
        t.column('CustNo', 'integer', format='zzzzzzzzz', initial='0',
                 column_label='CustNo',
                 help='Customer Number')
        t.column('CLI', 'character', format='x(12)', initial='',
                 label='A-Sub',
                 column_label='A-Sub',
                 help='A-Subscriber number')
        t.column('DeliStat', 'integer', format='9', initial='1',
                 column_label='DeliStat',
                 help='Delivery Status')
        t.column('Delitype', 'integer', format='>9', initial='0',
                 label='DeliType',
                 column_label='DeliType',
                 help='Delivere Type')
        t.column('DeliPara', 'character', format='x(40)', initial='',
                 label='DeliverParam',
                 column_label='DP',
                 help='Delivere Parameter')
        t.column('DeliMsg', 'character', format='x(255)', initial='',
                 label='Msg',
                 column_label='Msg',
                 help='Delivere message')
        t.column('ActStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Activated',
                 column_label='Activated',
                 help='Date and Time When Call Alarm was  activated',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('CLSeq', 'integer', format='>>>>>>>9', initial='0',
                 label='Seq',
                 column_label='Seq',
                 help='Call Limit Sequence')
        t.column('limit', 'integer', format='>>9', initial='0',
                 column_label='limit',
                 description='limit')
        t.column('CreditType', 'integer', format='>9', initial='0',
                 column_label='CreditType',
                 help='Credit Type')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('Orig', 'character', format='x(12)', initial='',
                 label='OrigNo',
                 column_label='',
                 help='Originating number')
        t.column('ActInterval', 'character', format='x(12)', initial='',
                 label='Activation Interval',
                 column_label='Act.Interval',
                 help='Time interval of day when CallAlarm handling is allowed')
        t.column('DeliStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Delivered',
                 column_label='Delivered',
                 help='Date and Time When Call Alarm was delivered',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.index('ActStamp', ['Brand', ('ActStamp', 'DESCENDING')], area='Sta_Index_1')
        t.index('CLI', ['Brand', 'CLI', 'DeliStat'], area='Sta_Index_1')
        t.index('CLI_s', ['CLI', 'DeliStat'], area='Sta_Index_1')
        t.index('CustNo', ['Brand', 'CustNo', 'DeliStat'], area='Sta_Index_1',
                primary=True)
        t.index('Delistat', ['Brand', 'DeliStat', 'Delitype', 'CustNo'], area='Sta_Index_1')
        t.index('TimeStamp', [('ActStamp', 'DESCENDING')], area='Sta_Index_1')

    def down(self):
        self.drop_table('CallAlarm')

