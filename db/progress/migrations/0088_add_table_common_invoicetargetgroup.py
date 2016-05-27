from gearbox.migrations import Migration

class AddTableInvoiceTargetGroup(Migration):

    database = "common"

    def up(self):
        t = self.table('InvoiceTargetGroup', area="Sta_Data_128", label="Invoice Target Group", dump_name="invoicetargetgroup", desc="Invoice target group")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('ITGroupID', 'integer', format=">>>>>>>>>>9", initial="0", max_width=4, label="Target Group ID", column_label="IT Group", position=3, order=20, help="Unique ID for target group")
        t.column('AgrCust', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Agreement Customer", column_label="Agr.Cust", position=4, order=30, help="Agreement customer")
        t.column('CustNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Invoice Customer", column_label="Inv.Cust", position=5, order=40, help="Invoice customer")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=6, order=50, help="Valid from")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=7, order=60, help="Valid to")
        t.column('DefaultGroup', 'logical', format="Yes/No", initial="no", max_width=1, label="Default Group", column_label="Default", position=8, order=70, help="Default group for new targets")
        t.column('DelType', 'integer', format=">>9", initial=self.unknown, max_width=4, label="Delivery Type", column_label="Del.Type", position=9, order=80)
        t.index('ITGroupID', [['ITGroupID']], area="Sta_Index_1", primary=True, unique=True)
        t.index('CustNum', [['CustNum'], ['ToDate', 'DESC']], area="Sta_Index_1")
        t.index('DefaultGroup', [['Brand'], ['AgrCust'], ['CustNum'], ['DefaultGroup']], area="Sta_Index_1")

    def down(self):
        self.drop_table('InvoiceTargetGroup')
