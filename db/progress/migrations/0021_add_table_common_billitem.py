from gearbox.migrations import Migration

class AddBillItem(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('BillItem', area='Sta_Data_64',
                       label='Billing Items',
                       dump_name='billitem',
                       desc='Billing items')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Billing Item',
                 column_label='BillItem',
                 help='Billing item code, max 16 characters')
        t.column('BIName', 'character', format='x(30)', initial='',
                 label='Bill.Item Name',
                 column_label='BI Name',
                 help='Billing item\'s name')
        t.column('AccNum', 'integer', format='>>>>>9', initial='0',
                 label='Account',
                 column_label='Account',
                 help='Account number')
        t.column('BIGroup', 'character', initial='',
                 label='Bill.Item Group',
                 column_label='BI Group',
                 help='Billing item group code')
        t.column('DispMPM', 'logical', initial='no',
                 label='Display MPM',
                 column_label='MPM',
                 help='Display MPM on specification reports')
        t.column('EUAccNum', 'integer', format='>>>>>9', initial='0',
                 label='EU Sales Account',
                 column_label='EU',
                 help='Account number for EU sales')
        t.column('FSAccNum', 'integer', format='>>>>>9', initial='0',
                 label='Foreign Sales Account',
                 column_label='Foreign',
                 help='Account number for sales outside EU')
        t.column('BillType', 'character', initial='',
                 label='Billing type',
                 column_label='Billing type')
        t.column('ISOrder', 'integer', format='>>9', initial='0',
                 label='Section Order',
                 column_label='Order',
                 help='Order within invoice section')
        t.column('VATCode', 'integer', format='z9', initial='1',
                 label='VAT code',
                 column_label='VAT code')
        t.column('TB1AccNum', 'integer', format='>>>>>9', initial='0',
                 label='TB1',
                 column_label='TB1',
                 help='Account for TB1')
        t.column('TB2AccNum', 'integer', format='>>>>>9', initial='0',
                 label='TB2',
                 column_label='TB2',
                 help='Account for TB2')
        t.column('InvSect', 'character', initial='',
                 label='Invoice Section',
                 column_label='Inv.Sect',
                 help='Code of an Invoice Section')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('EUVATCode', 'integer', format='z9', initial='0',
                 label='EU VAT Code',
                 column_label='EU VAT',
                 help='VAT code for EU sales')
        t.column('EUConAccNum', 'integer', format='>>>>>9', initial='0',
                 label='EU Cons. Sales',
                 column_label='EUCon',
                 help='Account number for EU sales with VAT (consumers)')
        t.column('CostCentre', 'character', initial='',
                 label='Cost Centre',
                 column_label='CCentre')
        t.column('AltAccNum', 'integer', format='>>>>>9', initial='0',
                 label='Own Use Account',
                 column_label='Own Use',
                 help='Account for own use')
        t.column('TaxClass', 'character', initial='',
                 label='Tax Class',
                 column_label='Class',
                 help='Tax class')
        t.column('SAPRid', 'character', initial='',
                 label='SAP Reporting ID',
                 column_label='SAP RID',
                 help='SAP reporting ID')
        t.column('VIPAccNum', 'integer', format='>>>>>>>9', initial='0',
                 label='VIP Account',
                 column_label='VIP Acc',
                 help='Account for VIP customers')
        t.column('OrderChannelOrder', 'integer', format='>>>>>>>>>9', initial='0',
                 label='Order Channel Order',
                 column_label='Order',
                 help='Presentation order in order channel')
        t.index('BIGroup', ['Brand', 'BIGroup', 'BillCode'], area='Sta_Index_2',
                unique=True)
        t.index('BillCode', ['Brand', 'BillCode'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('BIName', ['Brand', 'BIName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('BillItem')

