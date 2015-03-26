from gearbox.migrations import Migration

class AddSLGAnalyse(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('SLGAnalyse', area='Sta_Data_64',
                       dump_name='slganaly')
        t.column('BelongTo', 'logical', format='+/-', initial='TRUE',
                 column_label='BelongTo',
                 help='Belong to ServicelimitGroup')
        t.column('Clitype', 'character', initial='',
                 label='CLI Type',
                 column_label='CLIType')
        t.column('BillCode', 'character', format='x(16)', initial='',
                 label='Billing Item',
                 column_label='BillCode',
                 help='Billing item code, max 16 characters')
        t.column('CCN', 'integer', format='>>9', initial='0',
                 column_label='CCN')
        t.column('BDest', 'character', format='x(16)', initial='',
                 label='B-subNo',
                 column_label='B-subNo',
                 help='B-subscriber/destination ')
        t.column('ValidFrom', 'date', format='99-99-9999',
                 label='Valid From',
                 column_label='Valid From',
                 help='The date FROM which this SLG analyse will be used.')
        t.column('ValidTo', 'date', format='99-99-9999',
                 label='Valid To',
                 column_label='Valid To',
                 help='The date TO which this SLG analyse will be used.')
        t.column('ServiceLimitGroup', 'character', format='x(16)', initial='',
                 label='ServiceLimit Group',
                 column_label='ServiceLimitGroup',
                 help='ServiceLimitGroup')
        t.column('Prior', 'integer', format='>>9', initial='0',
                 column_label='Priority',
                 help='Priority')
        t.column('SLGAType', 'integer', format='9', initial='0',
                 label='Type',
                 column_label='Type',
                 help='Service type')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('BelongTo', ['Brand', 'BelongTo', 'Clitype', 'BillCode', 'CCN', 'BDest', 'Prior', ('ValidTo', 'DESCENDING')], area='Sta_Index_2')
        t.index('BillCode', ['Brand', 'BillCode', 'Clitype', ('ValidTo', 'DESCENDING')], area='Sta_Index_2',
                primary=True)
        t.index('CliType', ['Brand', 'Clitype', 'BillCode', ('ValidTo', 'DESCENDING')], area='Sta_Index_2')
        t.index('ServiceLimitGroup', ['Brand', 'ServiceLimitGroup', 'Clitype', ('ValidTo', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('SLGAnalyse')

