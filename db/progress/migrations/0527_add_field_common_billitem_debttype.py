from gearbox.migrations import Migration

class AddFieldDebtType(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('BillItem')
        t.column('DebtType', 'character', format="x(12)", initial="", label="Debt Type", column_label="DebtType", position=27, max_width=24, order=400, help="Debt type")
        
    def down(self):
        t = self.alter_table('BillItem')
        t.drop_column('DebtType')
        