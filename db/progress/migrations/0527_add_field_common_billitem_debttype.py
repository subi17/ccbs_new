from gearbox.migrations import Migration

class AddFieldDebtType(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('BillItem')
        t.column('DebtType', 'character', format="x(12)", initial="", label="Debt Type", column_label="DebtType", position=27, max_width=24, order=400, help="Debt type")
         t.column('ProfitType', 'character', format="x(12)", initial="", label="Profit Type", column_label="ProfitType", position=28, max_width=24, order=410, help="Profit type")

    def down(self):
        t = self.alter_table('BillItem')
        t.drop_column('DebtType')
        t.drop_column('ProfitType')
        