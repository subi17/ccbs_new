from gearbox.migrations import Migration

class AddTableCCN(Migration):

    database = "common"

    def up(self):
        t = self.table('CCN', area="Sta_Data_128", label="CCN", dump_name="ccn", desc="CCN")
        t.column('CCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="CCN", column_label="CCN", position=2, order=10, help="Call Case Number (CCN)")
        t.column('CCNName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Name", position=3, order=50, help="Name of CCN")
        t.column('RepCCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="Reporting CCN", column_label="RepCCN", position=4, order=60, help="Reporting CCN")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=70, help="Code Of Brand")
        t.index('CCN', [['Brand'], ['CCN']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CCNName', [['Brand'], ['CCNName']], area="Sta_Index_2")
        t.index('RepCCN', [['Brand'], ['RepCCN'], ['CCN']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CCN')
