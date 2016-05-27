from gearbox.migrations import Migration

class AddTableInvASub(Migration):

    database = "common"

    def up(self):
        t = self.table('InvASub', area="Dyn_Data_64", label="Invoiced Calls by CLI", dump_name="invasub", desc="Invoiced calls by CLI")
        t.column('Qty', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Amount", column_label="Amount", position=2, order=30, help="Amount of call to this CCN")
        t.column('Minutes', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="AmtMin", column_label="AmtMin", position=3, order=40, help="Amount of minutes to this CCN")
        t.column('InvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="InvoiceNo", column_label="InvoiceNo", position=4, order=10, help="Consecutive Invoice Number, 1 ... 99999999")
        t.column('Amt', 'decimal', format=">>>>>>9.99", decimals=6, initial="0", max_width=21, label="Value", column_label="Value", position=5, order=50, help="Value of calls to this CCN")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="A-Sub", column_label="A-Sub", position=6, order=60, help="A-Subscriber number")
        t.column('CCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="CCN", column_label="CCN", position=7, order=20, help="Consecutive country number")
        t.column('GenPrice', 'decimal', format="zz,zz9.99", decimals=6, initial="0", max_width=21, label="GenPrice", column_label="GenPrice", position=8, order=130, help="Call's general price")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="ProdCd", column_label="ProdCd", position=9, order=70, help="Product code, max 16 characters")
        t.column('InvSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="InvSeq", column_label="InvSeq", position=10, order=610, help="Calls invoice sequence")
        t.column('TariffNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="TariffID", column_label="TariffID", position=11, order=120, help="Tariff ID for rate used")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="DateFrom", column_label="DateFrom", position=13, order=630, help="FROM date (oldest call date)")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="DateTo", column_label="DateTo", position=14, order=640, help="TO date (latest call date)")
        t.column('DataAmt', 'decimal', format="->>>>>>>>>>>>", decimals=2, initial="0", max_width=17, label="Data Amount", column_label="Data", position=15, order=650, help="Transferred data amount")
        t.column('MPMAmt', 'decimal', format=">>>>>>9.99", decimals=6, initial="0", max_width=21, label="MPM Amount", column_label="MPM", position=16, order=660, help="Value of MPM")
        t.column('MpmRid', 'character', format="x(8)", initial="", max_width=16, label="Reporting ID", column_label="MpmRid", position=17, order=670, help="Reporting id")
        t.column('ServRid', 'character', format="x(8)", initial="", max_width=16, label="Service ID", column_label="ServRid", position=18, order=680, help="Service reporting id")
        t.column('SubInvNum', 'integer', format=">>9", initial="0", max_width=4, label="SubInvoice Number", column_label="SubInv", position=19, order=690, help="Sequential nbr of the subinvoice within the combined invoice")
        t.column('TBDurat', 'integer', format="zzzzz9", initial="0", max_width=84, label="TB Dur.", column_label="TB Dur.", extent=6, position=23, order=620, help="Duration (sec) within each timeband")
        t.index('InvNum', [['InvNum'], ['CLI'], ['CCN'], ['BillCode']], area="Dyn_Index_1", primary=True)

    def down(self):
        self.drop_table('InvASub')
