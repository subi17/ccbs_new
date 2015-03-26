from gearbox.migrations import Migration

class AddCoTarg(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CoTarg', area='Sta_Data_128',
                       dump_name='cotarg')
        t.column('CoRuleID', 'integer', format='>>>>>9', initial='0',
                 label='Rule ID',
                 column_label='RuleID',
                 help='Commissiong rule ID')
        t.column('TargType', 'character', format='X(1)', initial='',
                 label='Target Type',
                 column_label='Target Type',
                 help='Commission Target Type. Valid values are C, S or R',
                 description='Valid values are (C)ustomer, (S)alesman or (R)eseller')
        t.column('CoTarg', 'character', format='X(10)', initial='',
                 label='Commission Target',
                 column_label='Commission Target',
                 help='Commission Target Code based on Target type')
        t.column('RsLevel', 'integer', format='>9', initial='0',
                 label='Reseller Level',
                 column_label='RSLevel',
                 help='Salesman\'s level in reseller\'s organization')
        t.column('CoTargId', 'integer', format='>>>>>>>9', initial='0',
                 label='Target ID',
                 column_label='TargID',
                 help='Commission target ID')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('PromotedID', 'integer', format='>>>>>>>9', initial='0',
                 label='Promoted ID',
                 column_label='Prom.ID')
        t.column('PromotedCLI', 'character', format='x(12)', initial='',
                 label='Promoted MSISDN',
                 column_label='Prom.MSISDN')
        t.column('CommStatus', 'integer', format='>9', initial='0',
                 label='Commission Status',
                 column_label='Status',
                 help='Commission status')
        t.column('StatusReason', 'integer', format='>9', initial='0',
                 label='Status Reason',
                 column_label='Reason',
                 help='Reason for current status')
        t.column('HandledTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Handled',
                 help='Latest handling time')
        t.column('OrderId', 'integer', format='>>>>>>>>9', initial='0',
                 column_label='OrderId',
                 help='Order sequence number',
                 description='Order sequence number')
        t.column('CreatedTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 help='Added to commission queue')
        t.index('CommStatus', ['Brand', 'CommStatus', 'CoTarg'], area='Sta_Index_2')
        t.index('CoRuleID', ['Brand', 'CoRuleID', 'TargType', 'CoTarg'], area='Sta_Index_2',
                primary=True)
        t.index('CoTargID', ['CoTargId'], area='Sta_Index_2',
                unique=True)
        t.index('OrderId', ['Brand', 'OrderId'], area='Sta_Index_2')
        t.index('PromotedID', ['Brand', 'PromotedID'], area='Sta_Index_2')
        t.index('TargType', ['Brand', 'TargType', 'CoTarg'], area='Sta_Index_2')

    def down(self):
        self.drop_table('CoTarg')

