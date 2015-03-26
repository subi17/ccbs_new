from gearbox.migrations import Migration

class AddFATGroup(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('FATGroup', area='Sta_Data_64',
                       label='FAT Group',
                       dump_name='fatgroup',
                       desc='FAT Group for products')
        t.column('FtGrp', 'character', initial='',
                 label='FatGroup',
                 column_label='FtGrp',
                 help='Fat Group (for products)')
        t.column('FtgName', 'character', format='x(30)', initial='',
                 label='Name',
                 help='Group name')
        t.column('InvMemo', 'character', extent=5, format='x(60)', initial='',
                 label='Invoice Text',
                 column_label='Inv.Txt',
                 help='Text to invoice')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Billing Item',
                 column_label='B.Item',
                 help='Billing item')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('AmtLimit', 'decimal', decimals=2, format='>>>>>>9.99', initial='0',
                 label='Amount Limit',
                 column_label='Limit',
                 help='Max amount of FAT per CLI')
        t.column('FATType', 'integer', format='9', initial='0',
                 label='FATime Type',
                 column_label='Type',
                 help='FATime type',
                 description='0=calls, 1=fixed fees, 2=all')
        t.column('Transfer', 'logical', initial='no',
                 label='Transferrable',
                 column_label='Transfer',
                 help='Transferrable to next period')
        t.column('Amount', 'decimal', decimals=2, format='->>>>>>9.99', initial='0',
                 help='Amount per period')
        t.column('FATPerc', 'decimal', decimals=2, format='->>>>>9.99', initial='0',
                 label='Percentage',
                 column_label='Perc.',
                 help='FATime percentage')
        t.column('FatTarget', 'character', initial='',
                 label='FATime Target',
                 column_label='Target',
                 help='To whom FATime can be used')
        t.column('Priority', 'integer', format='>>9', initial='0',
                 column_label='Pri',
                 help='Relative priority to other FATimes')
        t.column('PeriodQty', 'integer', format='>9', initial='1',
                 label='Period Quantity',
                 column_label='Periods',
                 help='Period quantity')
        t.column('QtyUnit', 'character', initial='',
                 label='Qty Unit',
                 column_label='Unit',
                 help='Unit of the amount')
        t.column('Interval', 'integer', format='>9', initial='1',
                 help='Interval; number of months between events')
        t.column('GroupType', 'integer', format='>>9', initial='0',
                 label='Group Type',
                 column_label='GType',
                 help='FATime group type')
        t.column('ValidPeriods', 'integer', format='>>9', initial='0',
                 label='Valid Periods',
                 column_label='Valid',
                 help='How many periods is FATime valid for')
        t.index('FatType', ['Brand', 'FATType', 'FtGrp'], area='Sta_Index_2')
        t.index('FtgName', ['Brand', 'FtgName'], area='Sta_Index_2')
        t.index('FtGrp', ['Brand', 'FtGrp'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('GroupType', ['Brand', 'GroupType', 'FtGrp'], area='Sta_Index_2')
        t.index('Priority', ['Brand', ('Priority', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('FATGroup')

