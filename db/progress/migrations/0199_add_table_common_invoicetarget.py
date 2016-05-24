from gearbox.migrations import Migration

class AddTableInvoiceTarget(Migration):

    database = "common"

    def up(self):
        t = self.table('InvoiceTarget', area="Sta_Data_256", label="Invoice Target", dump_name="invoicetarget", desc="Invoice target")
        t.column('ITGroupID', 'integer', format=">>>>>>>>>>9", initial="0", max_width=4, label="Target Group ID", column_label="IT Group", position=2, order=10, help="Unique ID for target group")
        t.column('InvoiceTargetID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Invoice Target ID", column_label="Target ID", position=3, order=20, help="Unique id for invoice target")
        t.column('MsSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Subscr.ID", position=4, order=30, help="Subscription ID")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="From", position=5, order=40, help="Valid from")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="Valid To", column_label="To", position=6, order=50, help="Valid to")
        t.index('InvoiceTargetID', [['InvoiceTargetID']], area="Sta_Index_1", primary=True, unique=True)
        t.index('ITGroupID', [['ITGroupID'], ['ToDate', 'DESC']], area="Sta_Index_1")
        t.index('MsSeq', [['MsSeq'], ['ToDate', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('InvoiceTarget')
