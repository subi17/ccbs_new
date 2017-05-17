from gearbox.migrations import Migration

class AddTableAccessSchema(Migration):

    database = "common"

    def up(self):
        t = self.table('AccessSchema', area="Sta_Data_128", label="Access schemas", dump_name="schema", desc="Access schema definitions for limiting access to certain data")
        t.column('AccessSchemaSeq', 'integer', mandatory=True, format="->,>>>,>>9", initial="0", max_width=4, label="AccessSchemaSeq", column_label="AccessSchemaSeq", position=2, order=10)
        t.column('AScName', 'character', format="x(20)", initial="", max_width=40, label="Access Schema Name", column_label="Access Schema Name", position=3, order=20)
        t.column('AScType', 'integer', mandatory=True, format="->,>>>,>>9", initial="0", max_width=4, label="Access Schema Type", column_label="Access Schema Type", position=4, order=30, help="Type for the Access Schema rule rules")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('AccessSchemaSeq', [['AccessSchemaSeq']], area="Sta_Index_2", primary=True, unique=True)
        t.index('AScName', [['Brand'], ['AScName']], area="Sta_Index_2")
        t.index('ASCType', [['Brand'], ['AScType']], area="Sta_Index_2")

    def down(self):
        self.drop_table('AccessSchema')
