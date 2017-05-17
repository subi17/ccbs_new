from gearbox.migrations import Migration

class AddTableFeeModel(Migration):

    database = "common"

    def up(self):
        t = self.table('FeeModel', area="Sta_Data_64", label="Billing Event", dump_name="bevent", desc="A group of single and/or periodical fees")
        t.column('FeeModel', 'character', format="x(8)", initial="", max_width=16, label="BEvent", column_label="BEvent", position=2, order=10, help="An unique code for a Billing Event")
        t.column('FeeName', 'character', format="x(40)", initial="", max_width=80, label="Name", column_label="Name", position=3, order=20, help="Name of Billing Case")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1830, label="Memo", column_label="Memo", extent=15, position=4, order=30, help="Memo text")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.column('FMGroup', 'integer', format=">>9", initial="0", max_width=4, label="Group", column_label="Group", position=6, order=50, help="FeeModel Group")
        t.index('FeeModel', [['Brand'], ['FeeModel']], area="Sta_Index_2", primary=True, unique=True)
        t.index('FeeGroup', [['Brand'], ['FMGroup']], area="Sta_Index_2")
        t.index('FeeName', [['Brand'], ['FeeName'], ['FeeModel']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('FeeModel')
