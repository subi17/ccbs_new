from gearbox.migrations import Migration

class AddOrderAccessory(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('OrderAccessory', area='Sta_Data_32',
                       dump_name='orderacc')
        t.column('OrderId', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 column_label='OrderId',
                 description='Order sequence number')
        t.column('Amount', 'decimal', decimals=2, format='>>>>9.99', initial='0',
                 column_label='Amount')
        t.column('VatAmount', 'decimal', decimals=2, format='>>>>9.99', initial='0',
                 column_label='VatAmount')
        t.column('ProductCode', 'character', initial='',
                 column_label='ProductCode')
        t.column('IMEI', 'character', format='x(12)', initial='',
                 column_label='IMEI')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('Discount', 'decimal', decimals=2, format='->>>>9.99', initial='0',
                 help='Discount amount')
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
                 label='SIM Lock Code')
        t.column('TerminalType', 'integer', format='>9', initial='0',
                 label='Terminal Type',
                 column_label='Term.Type',
                 help='Type of terminal')
        t.index('OrderId', ['Brand', 'OrderId'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('OrderAccessory')

