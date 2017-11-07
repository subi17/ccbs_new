from gearbox.migrations import Migration

class AddTableOperator(Migration):

    database = "common"

    def up(self):
        t = self.table('Operator', area="Sta_Data_128", label="Operators", dump_name="operator", desc="Operators")
        t.column('Operator', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="OpCode", column_label="OpCode", position=2, order=10, help="Operator's code, 1 - 8 characters")
        t.column('OperName', 'character', format="x(40)", initial="", max_width=80, label="OperName", column_label="OperName", position=3, order=20, help="Operator's name")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=4, order=30, help="Memotext")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=5, order=40, help="Operators customer number (used for collecting indirect calls)")
        t.column('Prefix', 'character', format="x(8)", initial="", max_width=16, label="Pref", column_label="Pref", position=6, order=50, help="Prefix used with number portability")
        t.index('Operator', [['Operator'], ['OperName']], area="Sta_Index_2", primary=True, unique=True)
        t.index('OperName', [['OperName'], ['Operator']], area="Sta_Index_2", unique=True)
        t.index('Prefix', [['Prefix'], ['Operator']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('Operator')
