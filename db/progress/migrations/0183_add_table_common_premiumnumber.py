from gearbox.migrations import Migration

class AddTablePremiumNumber(Migration):

    database = "common"

    def up(self):
        t = self.table('PremiumNumber', area="Sta_Data_128", label="Premium Numbers", dump_name="premiumnumber", desc="Premium number descriptions")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('BNumberPrefix', 'character', format="x(12)", initial="", max_width=24, label="B Number Prefix", column_label="BNumberPrefix", position=3, order=20)
        t.column('OperatorName', 'character', format="x(40)", initial="", max_width=80, label="Operator Name", column_label="OperName", position=4, order=30, help="Premium service operator name")
        t.column('ValidFrom', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="ValidFrom", position=5, order=40, help="Date when configuration becomes active")
        t.column('ValidTo', 'date', format="99-99-99", max_width=4, label="Valid To", column_label="ValidTo", position=6, order=50, help="Date after configuration becomes inactive")
        t.index('BNumberPrefix', [['Brand'], ['BNumberPrefix'], ['ValidTo', 'DESC']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('PremiumNumber')
