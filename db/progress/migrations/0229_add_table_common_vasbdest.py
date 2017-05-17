from gearbox.migrations import Migration

class AddTableVASBdest(Migration):

    database = "common"

    def up(self):
        t = self.table('VASBdest', area="Sta_Data_256", dump_name="vasbdest")
        t.column('OperID', 'character', format="x(8)", initial="", max_width=16, label="Operator ID", column_label="Operator ID", position=2, order=10, help="Operator ID")
        t.column('BDest', 'character', mandatory=True, format="x(25)", initial="", max_width=50, label="B-subNo", column_label="B-subNo", position=3, order=20, help="B-number")
        t.column('InvEvent', 'integer', format="9", initial="0", max_width=4, label="InvEvent", column_label="Invoice Event", position=4, order=30, help="Invoicable Event")
        t.index('bdest', [['BDest']], area="Sta_Index_2", primary=True, unique=True)
        t.index('operid', [['OperID']], area="Sta_Index_2")

    def down(self):
        self.drop_table('VASBdest')
