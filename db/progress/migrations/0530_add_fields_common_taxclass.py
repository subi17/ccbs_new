from gearbox.migrations import Migration

class AddFieldTaxClass(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('TaxClass')
        t.column('isActive', 'logical', format="yes/no", initial="no", label="Active", column_label="Active", position=4, max_width=1, order=30, help="Is TaxClass Active or not")
        t.column('TCType', 'character', format="x(12)", initial="", label="is Active", column_label="is Active", position=5, max_width=24, order=40, help="Type of TaxClass")
        t.column('amount', 'decimal', format="->>,>>9.99", initial="0", label="Amount", column_label="Amount", position=6, max_width=17, order=50, help="Tax Amount")
        
    def down(self):
        t = self.alter_table('TaxClass')
        t.drop_column('Active')
        t.drop_column('type')        
        t.drop_column('amount')