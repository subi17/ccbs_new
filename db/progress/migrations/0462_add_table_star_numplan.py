from gearbox.migrations import Migration

class AddTableNumPlan(Migration):

    database = "star"

    def up(self):
        t = self.table('NumPlan', area="Sta_Data_256", label="NumPlan", dump_name="numplan", desc="Numbering plan")
        t.column('AreaCode', 'character', format="x(4)", initial="", max_width=8, label="Area", column_label="Area", position=2, order=10, help="Area Code")
        t.column('Prefix', 'character', format="x(6)", initial="", max_width=12, label="Series", column_label="Series", position=3, order=20, help="1 - 4 firsts numbers in phonenumber after the areacode")
        t.column('Operator', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="Operator", column_label="Operator", position=4, order=30, help="Operator code, 1 - 8 characters")
        t.column('State', 'character', format="x(16)", initial="", max_width=32, label="Status", column_label="Status", position=5, order=40, help="Status of number series")
        t.column('NumLength', 'integer', format="z9", initial="0", max_width=4, label="Length", column_label="Length", position=6, order=50, help="""Total length of tel. number (without first """"0"""")""")
        t.index('Prefix', [['Prefix']], area="Sta_Index_2", primary=True, unique=True)
        t.index('AreaCode', [['AreaCode'], ['Prefix']], area="Sta_Index_2")
        t.index('Operator', [['Operator'], ['AreaCode'], ['Prefix']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('NumPlan')
