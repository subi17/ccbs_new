from gearbox.migrations import Migration

class AddSubsTerminal(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('SubsTerminal', area='Sta_Data_64',
                       label='Subscription Terminal',
                       dump_name='substerminal',
                       desc='Terminal of a subscription')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('MSSeq', 'integer', format='>>>>>>>9', initial='0',
                 label='Subscription ID',
                 column_label='Subs.ID')
        t.column('OrderId', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Order ID',
                 column_label='Order')
        t.column('TerminalID', 'integer', format='>>>>>>>>>9', initial='0',
                 label='Terminal ID',
                 column_label='Term.ID',
                 description='unique sequence nbr')
        t.column('IMEI', 'character', format='x(15)', initial='',
                 column_label='IMEI',
                 help='IMEI code')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Billing Item',
                 column_label='Bill.Item',
                 help='Billing item code')
        t.column('PurchaseTS', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Purchased',
                 help='Time of purchase')
        t.column('Model', 'character', format='x(30)', initial='',
                 column_label='Model',
                 help='Terminal model')
        t.column('ModelColor', 'character', format='x(30)', initial='',
                 column_label='ModelColor',
                 help='Terminal color')
        t.column('Manufacturer', 'character', format='x(30)', initial='',
                 column_label='Manufacturer',
                 help='Terminal manufacturer')
        t.column('SIMLockCode', 'character', format='x(20)', initial='',
                 label='SIM Lock Code',
                 help='SIM lock code')
        t.column('PerContractID', 'integer', format='>>>>>>>>>9', initial='0',
                 label='Periodical Contract ID',
                 column_label='Per.Contr.',
                 help='Periodical contract ID',
                 description='DCCLI')
        t.column('SIMChecked', 'logical', initial='no',
                 label='SIM Checked',
                 column_label='SIMCheck',
                 help='SIM checked')
        t.column('TerminalType', 'integer', format='>9', initial='0',
                 label='Terminal Type',
                 column_label='Term.Type',
                 help='Type of terminal')
        t.index('MsSeq', ['MSSeq', ('PurchaseTS', 'DESCENDING')], area='Sta_Index_1')
        t.index('OrderId', ['Brand', 'OrderId'], area='Sta_Index_1')
        t.index('TerminalID', ['TerminalID'], area='Sta_Index_1',
                primary=True, unique=True)

    def down(self):
        self.drop_table('SubsTerminal')

