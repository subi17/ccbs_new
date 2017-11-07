from gearbox.migrations import Migration

class AddTableInvSect(Migration):

    database = "common"

    def up(self):
        t = self.table('InvSect', area="Sta_Data_2_256", label="Invoice sections", dump_name="invsect")
        t.column('InvSect', 'character', format="x(8)", initial="", max_width=16, label="Invoice Section", column_label="Invoice Section", position=2, order=10, help="Code of an Invoice Section")
        t.column('ISName', 'character', format="x(40)", initial="", max_width=80, label="Section Name", column_label="Section Name", position=3, order=20, help="Name of an Invoice Section")
        t.column('ISValue', 'logical', format="Calls/Others", initial="no", max_width=1, label="Contains", column_label="Contains", position=4, order=30, help="Does this section contain (C)alls or (O)ther fees")
        t.column('PrintHead', 'logical', format="Yes/No", initial="no", max_width=1, label="Print to Invoice", column_label="Print", position=5, order=40, help="Print header text to invoice")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('InvSect', [['Brand'], ['InvSect']], area="Sta_Index_4", primary=True, unique=True)
        t.index('ISName', [['Brand'], ['ISName'], ['InvSect']], area="Sta_Index_4", unique=True)

    def down(self):
        self.drop_table('InvSect')
