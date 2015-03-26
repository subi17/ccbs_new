from gearbox.migrations import Migration

class AddAccessSchemaRule(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('AccessSchemaRule', area='Sta_Data_256',
                       label='Access Schema Rule',
                       dump_name='accesssc')
        t.column('AccessSchemaRuleSeq', 'integer', mandatory=True, initial='0')
        t.column('AccessSchemaSeq', 'integer', mandatory=True, initial='0',
                 column_label='AccessSchemaSeq')
        t.column('Rule', 'character', format='x(30)', initial='')
        t.column('Order', 'integer', mandatory=True, initial='0')
        t.column('Action', 'integer', mandatory=True, initial='0')
        t.index('AccessSchemaRuleSeq', ['AccessSchemaRuleSeq'], area='Sta_Index_2',
                unique=True)
        t.index('AccessSchemaSeq', ['AccessSchemaSeq', 'AccessSchemaRuleSeq'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('AccessSchemaRule')

