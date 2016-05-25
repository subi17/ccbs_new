from gearbox.migrations import Migration

class AddTableReseller(Migration):

    database = "common"

    def up(self):
        t = self.table('Reseller', area="Sta_Data_128", label="Resellers", dump_name="reseller", desc="Resellers")
        t.column('Reseller', 'character', format="x(8)", initial="", max_width=16, label="Code", column_label="Code", position=2, order=10, help="""An unique code for a reseller; maximum 8 characters""")
        t.column('RsName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Name", position=3, order=20, help="Reseller's name")
        t.column('Salesman', 'character', format="x(8)", initial="", max_width=16, label="Salesman", column_label="Salesman", position=4, order=30, help="Code of that salesman who is responsible of this reseller")
        t.column('CommPerc', 'decimal', format="z9.9", decimals=2, initial="0", max_width=17, label="Com%", column_label="Com%", position=5, order=40, help="Amount of Reseller's Commission (%)")
        t.column('Address', 'character', format="x(30)", initial="", max_width=186, label="Address", column_label="Address", extent=3, position=6, order=50, help="Resellers address")
        t.column('EMail', 'character', format="x(40)", initial="", max_width=80, label="Email", column_label="Email", position=7, order=60, help="Resellers Email address")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=8, order=70, help="Code Of Brand")
        t.column('EntityCode', 'integer', format=">>9", initial="?", max_width=4, label="EntityCode", column_label="Entity Code", position=9, order=80)
        t.column('Active', 'logical', format="Yes/No", initial="Yes", max_width=1, label="Active", column_label="Active", position=10, order=90)
        t.column('FUC1', 'character', format="x(10)", initial="", max_width=20, label="FUC1", column_label="FUC1", position=11, order=100)
        t.column('FUC2', 'character', format="x(10)", initial="", max_width=20, label="FUC2", column_label="FUC2", position=12, order=110)
        t.index('Reseller', [['Brand'], ['Reseller']], area="Sta_Index_2", primary=True, unique=True)
        t.index('RsName', [['Brand'], ['RsName'], ['Reseller']], area="Sta_Index_2", unique=True)
        t.index('Salesman', [['Brand'], ['Salesman'], ['Reseller']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('Reseller')
