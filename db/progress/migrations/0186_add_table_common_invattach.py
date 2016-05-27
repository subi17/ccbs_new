from gearbox.migrations import Migration

class AddTableInvAttach(Migration):

    database = "common"

    def up(self):
        t = self.table('InvAttach', area="Sta_Data_256", label="Invoice Attachments", dump_name="invattac", desc="Attachment definitions for EPL printing of invoices")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Cust", position=2, order=10, help="Customer number")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=3, order=20, help="Valid from")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=4, order=30, help="Valid to")
        t.column('Attach1', 'integer', format=">", initial="0", max_width=4, label="Attachment 1", column_label="Att1", position=5, order=40, help="Attachment 1")
        t.column('Attach2', 'integer', format=">", initial="0", max_width=4, label="Attachment 2", column_label="Att2", position=6, order=50, help="Attachment 2")
        t.column('Attach3', 'integer', format=">", initial="0", max_width=4, label="Attachment 3", column_label="Att3", position=7, order=60, help="Attachment 3")
        t.column('Attach4', 'integer', format=">", initial="0", max_width=4, label="Attachment 4", column_label="Att4", position=8, order=70, help="Attachment 4")
        t.column('PrintInv', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Printed to Invoice", column_label="Invoice", position=9, order=80, help="Invoice to which attachments were printed")
        t.column('PrintStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Print Time", column_label="Time", position=10, order=90, help="Time when invoice was printed (with attachment data)")
        t.index('CustNum', [['CustNum'], ['ToDate', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('PrintStamp', [['PrintStamp', 'DESC']], area="Sta_Index_2")
        t.index('ToDate', [['ToDate', 'DESC'], ['CustNum']], area="Sta_Index_2")

    def down(self):
        self.drop_table('InvAttach')
