from gearbox.migrations import Migration

class AddTableFileExpLog(Migration):

    database = "common"

    def up(self):
        t = self.table('FileExpLog', area="Sta_Data_256", label="Transfer file", dump_name="fileexpl", desc="Transfer file for bookkeeping")
        t.column('TransType', 'character', format="x(8)", initial="", max_width=16, label="Type", column_label="Type", position=2, order=10, help="Type of transfer file")
        t.column('TransNum', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Xfer file", column_label="Xfer file", position=3, order=20, help="Consecutive number of transfer file (for bookkeeping)")
        t.column('TransDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date", column_label="Date", position=4, order=30, help="Date when file was created")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('TransType', [['Brand'], ['TransType'], ['TransNum', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('TransDate', [['Brand'], ['TransDate', 'DESC'], ['TransType', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('FileExpLog')
