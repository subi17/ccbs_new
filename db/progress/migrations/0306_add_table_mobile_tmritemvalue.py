from gearbox.migrations import Migration

class AddTMRItemValue(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('TMRItemValue', area='Sta_Data_256',
                       label='TM Rule Item Values',
                       dump_name='tmritemvalue',
                       desc='TM rule item values\
')
        t.column('TMRuleSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='Rule Sequence',
                 column_label='Seq',
                 help='Rule ID')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From',
                 help='Date when rule becomes effective')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='Date when rule expires')
        t.column('CounterItemValues', 'character', format='x(40)', initial='',
                 label='Item Values',
                 column_label='Values',
                 help='List of item values used to collect the counter')
        t.index('CounterItemValues', ['TMRuleSeq', 'CounterItemValues', ('ToDate', 'DESCENDING')], area='Sta_Index_1',
                primary=True)
        t.index('ToDate', ['TMRuleSeq', ('ToDate', 'DESCENDING')], area='Sta_Index_1')

    def down(self):
        self.drop_table('TMRItemValue')

