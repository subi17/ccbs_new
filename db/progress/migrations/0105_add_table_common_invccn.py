from gearbox.migrations import Migration

class AddTableInvCCN(Migration):

    database = "common"

    def up(self):
        t = self.table('InvCCN', area="Dyn_Data_256", label="Invoiced Calls by CCN", dump_name="invccn", desc="Invoiced calls by CCN")
        t.column('InvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="InvoiceNo", column_label="InvoiceNo", position=2, order=10, help="Consecutive Invoicenumber, 1 ... 99999999")
        t.column('CCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="CCN", column_label="CCN", position=3, order=20, help="Consecutive country number")
        t.column('Qty', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Amount", column_label="Amount", position=4, order=30, help="Amount of call to this CCN")
        t.column('Minutes', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="AmtMin", column_label="AmtMin", position=5, order=40, help="Amount of minutes to this CCN")
        t.column('Amt', 'decimal', format=">>>>>>9.99", decimals=6, initial="0", max_width=21, label="Value", column_label="Value", position=6, order=50, help="Value of calls to this CCN")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="ProdCd", column_label="ProdCd", position=7, order=70, help="Product code, max 16 characters")
        t.column('GenPrice', 'decimal', format="zz,zz9.99", decimals=6, initial="0", max_width=21, label="GenPrice", column_label="GenPrice", position=8, order=130, help="Call's general price")
        t.column('InvSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="InvSeq", column_label="InvSeq", position=9, order=610, help="Calls invoice sequence")
        t.column('TBDurat', 'integer', format="zzzzz9", initial="0", max_width=84, label="TB Dur.", column_label="TB Dur.", extent=6, position=10, order=620, help="Duration (sec) within each timeband")
        t.column('TariffNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="TariffID", column_label="TariffID", position=11, order=120, help="Tariff ID for rate used")
        t.column('DataAmt', 'decimal', format="->>>>>>>>>>>>", decimals=2, initial="0", max_width=17, label="Data Amount", column_label="Data", position=12, order=630, help="Transferred data amount")
        t.column('SubInvNum', 'integer', format=">>9", initial="0", max_width=4, label="SubInvoice Number", column_label="SubInv", position=13, order=140, help="Sequential nbr of the subinvoice within the combined invoice")
        t.index('InvNum', [['InvNum'], ['CCN'], ['BillCode'], ['TariffNum']], area="Dyn_Index_1", primary=True)
        t.index('SubInvNum', [['InvNum'], ['SubInvNum']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('InvCCN')
