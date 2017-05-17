from gearbox.migrations import Migration

class AddTableCCRule(Migration):

    database = "common"

    def up(self):
        t = self.table('CCRule', area="Sta_Data_256", label="Cost Centre Rules", dump_name="ccrule", desc='''Rules for determining cost centre
''')
        t.column('Category', 'character', format="x(4)", initial="", max_width=8, label="Category", column_label="Category", position=2, order=10, help="Customer category code")
        t.column('BIGroup', 'character', format="x(8)", initial="", max_width=16, label="Product group", column_label="Prod.grp", position=3, order=20, help="Product group")
        t.column('ValidFrom', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Effective date", column_label="Eff.date", position=4, order=30, help="Date when rule becomes effective")
        t.column('CostCentre', 'character', format="x(8)", initial="", max_width=16, label="Cost Centre", column_label="Cc", position=5, order=40, help="Cost centre to be used")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('Category', [['Brand'], ['Category'], ['BIGroup'], ['ValidFrom', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Pgcode', [['Brand'], ['BIGroup'], ['Category'], ['ValidFrom', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CCRule')
