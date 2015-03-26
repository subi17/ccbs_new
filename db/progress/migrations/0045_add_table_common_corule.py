from gearbox.migrations import Migration

class AddCoRule(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CoRule', area='Sta_Data_64',
                       label='Commission Rule',
                       dump_name='corule',
                       desc='Defined rules for the commission to be paid to Salesman.')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('BillCode', 'character', mandatory=True, format='x(16)', initial='',
                 label='Billing Item',
                 column_label='BillItem',
                 help='Billing Item code')
        t.column('coFrom', 'date', format='99-99-9999',
                 label='Commission From',
                 column_label='From',
                 help='Valid from')
        t.column('coTo', 'date', format='99-99-9999',
                 label='Commission To',
                 column_label='To',
                 help='Valid to')
        t.column('coNoInst', 'integer', format='>9', initial='0',
                 label='No. of Instalments',
                 column_label='No. of Inst.',
                 help='No. of Instalments in which fixed amount has to be paid.')
        t.column('coInterval', 'integer', format='>9', initial='0',
                 label='Interval',
                 column_label='Interval',
                 help='Interval (months) between commission payments')
        t.column('CoRuleID', 'integer', format='>>>>>9', initial='0',
                 label='Rule ID',
                 column_label='RuleID',
                 help='Commission rule ID')
        t.column('CommPoint', 'integer', format='9', initial='0',
                 label='Commission Point',
                 column_label='Point',
                 help='Point from where on commission is payable')
        t.column('AmtBilled', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 label='Amount billed',
                 column_label='Amt bill.',
                 help='Minimum amount that must be billed before commission is paid')
        t.column('QtyPaidInv', 'integer', format='>>9', initial='0',
                 label='Qty of paid inv.',
                 column_label='Qty paid',
                 help='Minimum quantity of paid invoices before commission is paid')
        t.column('CCN', 'integer', mandatory=True, format='zz9', initial='0',
                 column_label='CCN',
                 help='Call case number for calls')
        t.column('RuleDesc', 'character', format='x(30)', initial='',
                 label='Description',
                 column_label='Rule',
                 help='Rule description')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('BasisType', 'integer', format='9', initial='0',
                 label='Basis Type',
                 column_label='Basis',
                 help='Basis type (e.g. amount or subscription qty)')
        t.column('OpenDays', 'integer', format='>>>>9', initial='0',
                 label='Open Days',
                 column_label='Open',
                 help='Days that subscription must have been open after activation')
        t.column('ParentRule', 'integer', initial='0',
                 label='Parent Rule',
                 column_label='Parent',
                 help='Parent rule, that must have been active before this activates')
        t.column('RuleType', 'integer', format='9', initial='0',
                 label='Rule Type',
                 column_label='Type',
                 help='Commission rule type')
        t.column('FtGrp', 'character', initial='',
                 label='FatGroup',
                 column_label='FtGrp',
                 help='Fat Group')
        t.column('CLIType', 'character', format='x(12)', initial='',
                 label='CLI Type',
                 column_label='Type',
                 help='Type of the CLI')
        t.column('PayType', 'integer', format='9', initial='0',
                 label='Payment Type',
                 column_label='PayType',
                 help='Payment type',
                 description='postpaid, prepaid')
        t.column('AllowedDNI', 'character', format='x(30)', initial='',
                 label='Allowed DNIs',
                 column_label='DNIs',
                 help='List of allowed DNIs')
        t.column('PPReqPrefix', 'character', initial='',
                 label='Topup Prefix',
                 column_label='T.Prefix',
                 help='Prefix to be used for topup')
        t.column('PPSource', 'character', initial='',
                 label='Topup Source',
                 column_label='T.Source',
                 help='Source to be used for topup')
        t.column('CreationSMS', 'character', format='x(12)', initial='',
                 label='SMS On Creation',
                 column_label='CreationSMS',
                 help='SMS that is sent when entry is created to commission queue')
        t.column('ActivationSMS', 'character', format='x(12)', initial='',
                 label='Activation SMS',
                 column_label='Act.SMS',
                 help='SMS that is sent when commission is activated')
        t.column('CommAmount', 'decimal', decimals=2, format='>>>>>>>9.99', initial='0',
                 label='Commission Amount',
                 column_label='Amount',
                 help='Commission amount')
        t.column('MaxPendingDays', 'integer', format='>>>9', initial='0',
                 label='Max. Pending Days',
                 column_label='Max.Pending',
                 help='Maximum nbr of days that a commission can be pending')
        t.column('Priority', 'integer', format='>>9', initial='0',
                 column_label='Pri',
                 help='Relative priority to other rules')
        t.index('CoFrom', ['Brand', ('coFrom', 'DESCENDING'), 'CustNum'], area='Sta_Index_2')
        t.index('CoRuleID', ['Brand', 'CoRuleID'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CustNum', ['Brand', 'CustNum', 'RuleDesc'], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum', 'RuleDesc'], area='Sta_Index_2')
        t.index('RuleDesc', ['Brand', 'RuleDesc', 'CustNum'], area='Sta_Index_2')
        t.index('RuleType', ['Brand', 'RuleType', ('coFrom', 'DESCENDING'), 'CoRuleID'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CoRule')

