from gearbox.migrations import Migration

class AddTablePaymLog(Migration):

    database = "common"

    def up(self):
        t = self.table('PaymLog', area="Sta_Data_64", label="Payment Log", dump_name="paymlog", desc="Payment log")
        t.column('BookDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="BookDate", column_label="BookDate", position=2, order=10, help="Date When a Payment File was Booked")
        t.column('UserCode', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="User ID", column_label="User ID", position=3, order=20, help="User ID of log event")
        t.column('PaymFile', 'character', format="x(24)", initial="", max_width=48, label="File Identifier", column_label="FileId", position=4, order=30, help="Identifier of Payment File")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('bookdate', [['Brand'], ['BookDate'], ['PaymFile']], area="Sta_Index_2", primary=True, unique=True)
        t.index('PaymFile', [['Brand'], ['PaymFile'], ['BookDate']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('PaymLog')
