from gearbox.migrations import Migration

class AddCoEvent(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CoEvent', area='Sta_Data_64',
                       label='Commission Event',
                       dump_name='coevent',
                       desc='Commission Event')
        t.column('HostTable', 'character', mandatory=True, format='x(12)', initial='',
                 column_label='Table',
                 help='Name of table for which event is calculated')
        t.column('HostKey', 'character', format='x(10)', initial='',
                 label='Host Key',
                 column_label='Key',
                 help='Key of table for which event is calculated')
        t.column('CoRuleID', 'integer', format='>>>>>9', initial='0',
                 label='Rule ID',
                 column_label='RuleID',
                 help='Commission rule ID')
        t.column('CalcDate', 'date', format='99-99-99',
                 label='Calc. Date',
                 column_label='Date',
                 help='Date when event was created')
        t.column('Salesman', 'character', initial='',
                 column_label='Sman',
                 help='Salesman for which commission is paid')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer for which commission is paid')
        t.column('CommAmt', 'decimal', decimals=2, format='->>>>>>>9.99', initial='0',
                 label='Commission',
                 column_label='Amt',
                 help='Commission amount')
        t.column('PaymDate', 'date', format='99-99-99',
                 label='Payment Date',
                 column_label='Paid',
                 help='Date when commission has been paid out')
        t.column('CoEventID', 'integer', initial='0',
                 label='EventID',
                 column_label='ID',
                 help='Unique event ID')
        t.column('CoTargId', 'integer', format='>>>>>>>9', initial='0',
                 label='Target ID',
                 column_label='TargID',
                 help='Commission target ID')
        t.column('CommFrom', 'date', format='99-99-99',
                 label='From Date',
                 column_label='From',
                 help='Commission is based on events from this date onwards')
        t.column('CommTo', 'date', format='99-99-99',
                 label='To Date',
                 column_label='To',
                 help='Commission is based on events till this date')
        t.column('BaseAmt', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Base Amount',
                 column_label='Base',
                 help='Base amount for commission (% is calculated from this)')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('CoAmt', 'decimal', decimals=2, initial='0',
                 label='Fixed Amount',
                 column_label='Fixed',
                 help='Fixed commission amount')
        t.column('CoPerc', 'decimal', decimals=2, format='>9.99', initial='0',
                 label='Commission %',
                 column_label='Comm%',
                 help='Commission percentage')
        t.index('CalcDate', ['Brand', 'CalcDate', 'CoEventID'], area='Sta_Index_2')
        t.index('CoEventID', ['CoEventID'], area='Sta_Index_2',
                unique=True)
        t.index('CoRuleID', ['Brand', 'CoRuleID'], area='Sta_Index_2')
        t.index('CoTargID', ['CoTargId'], area='Sta_Index_2')
        t.index('CustNum', ['Brand', 'CustNum'], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum'], area='Sta_Index_2')
        t.index('HostTable', ['Brand', 'HostTable', 'HostKey'], area='Sta_Index_2',
                primary=True)
        t.index('PaymDate', ['Brand', ('PaymDate', 'DESCENDING'), 'CoEventID'], area='Sta_Index_2')
        t.index('Salesman', ['Brand', 'Salesman', ('PaymDate', 'DESCENDING'), 'CoEventID'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CoEvent')

