from gearbox.migrations import Migration

class AddTableAccessSchemaRule(Migration):

    database = "common"

    def up(self):
        t = self.table('AccessSchemaRule', area="Sta_Data_2_256", label="Access Schema Rule", dump_name="accesssc", file_misc26="")
        t.column('AccessSchemaRuleSeq', 'integer', mandatory=True, format="->,>>>,>>9", initial="0", max_width=4, position=2, order=10)
        t.column('AccessSchemaSeq', 'integer', mandatory=True, format="->,>>>,>>9", initial="0", max_width=4, label="AccessSchemaSeq", column_label="AccessSchemaSeq", position=3, order=20)
        t.column('Rule', 'character', format="x(30)", initial="", max_width=60, position=4, order=30)
        t.column('Order', 'integer', mandatory=True, format="->,>>>,>>9", initial="0", max_width=4, position=5, order=40)
        t.column('Action', 'integer', mandatory=True, format="->,>>>,>>9", initial="0", max_width=4, position=6, order=50)
        t.index('AccessSchemaSeq', [['AccessSchemaSeq'], ['AccessSchemaRuleSeq']], area="Sta_Index_3", primary=True)
        t.index('AccessSchemaRuleSeq', [['AccessSchemaRuleSeq']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('AccessSchemaRule')
