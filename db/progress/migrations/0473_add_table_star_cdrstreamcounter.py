from gearbox.migrations import Migration

class AddTableCDRStreamCounter(Migration):

    database = "star"

    def up(self):
        t = self.table('CDRStreamCounter', area="Dyn_Data_128", label="CDR Stream Counter", dump_name="cdrstreamcounter", desc="CDR Stream Counter")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('ImportDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Import Date", column_label="Import", position=3, order=20, help="Import date")
        t.column('MSCID', 'character', format="x(8)", initial="", max_width=16, label="MSCID", position=4, order=30, help="Mobile switching center")
        t.column('OnlineStream', 'integer', format=">>9", initial="0", max_width=4, label="Online Stream", column_label="Stream", position=5, order=40, help="Stream from which CDR was read")
        t.column('CounterType', 'character', format="x(8)", initial="", max_width=16, label="Counter Type", column_label="Type", position=6, order=50, help="Counter type")
        t.column('CDRQty', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Quantity", column_label="Qty", position=7, order=60, help="CDR quantity")
        t.index('ImportDate', [['Brand'], ['ImportDate'], ['CounterType'], ['MSCID'], ['OnlineStream']], area="Dyn_Index_1", primary=True, unique=True)

    def down(self):
        self.drop_table('CDRStreamCounter')
