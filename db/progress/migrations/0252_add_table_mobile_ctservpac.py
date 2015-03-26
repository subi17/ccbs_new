from gearbox.migrations import Migration

class AddCTServPac(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('CTServPac', area='Sta_Data_128',
                       label='CLIType Service Packages',
                       dump_name='ctservpa',
                       desc='Service Packages of a CLI type')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('CLIType', 'character', initial='',
                 label='CLI Type',
                 column_label='CLIType',
                 help='CLI type')
        t.column('ServPac', 'character', format='x(12)', initial='',
                 label='Service Package',
                 column_label='ServPack',
                 help='Service package code')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From Date',
                 column_label='From',
                 help='Valid from date')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To Date',
                 column_label='To',
                 help='Valid to date')
        t.column('ServiceLimit', 'character', format='x(16)', initial='',
                 label='Service Limit',
                 column_label='SLimit',
                 help='Service limit group')
        t.column('ServType', 'integer', format='9', initial='0',
                 label='Package Type',
                 column_label='Type',
                 help='Package type')
        t.index('CLIType', ['Brand', 'CLIType', 'ServPac', ('FromDate', 'DESCENDING')], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('ServPac', ['Brand', 'ServPac', 'CLIType', ('FromDate', 'DESCENDING')], area='Sta_Index_3')

    def down(self):
        self.drop_table('CTServPac')

