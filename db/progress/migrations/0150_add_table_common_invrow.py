from gearbox.migrations import Migration

class AddTableInvRow(Migration):

    database = "common"

    def up(self):
        t = self.table('InvRow', area="Dyn_Data_64", label="Invoice Rows", table_trigger=[{'crc': '?', 'procedure': 'rd-invrow.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-invrow.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="invrow", desc="Invoice rows")
        t.column('InvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="InvoiceNo", column_label="InvoiceNo", position=2, order=10, help="Consecutive Invoice Number, 1 ... 99999999")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Product", column_label="Product", position=3, order=20, help="Product number")
        t.column('GrossAmt', 'decimal', format="-zzzzz9.999", decimals=3, initial="0", max_width=18, label="TotBrutto", column_label="TotBrutto", position=4, order=30, help="Total amount by product exl. discount")
        t.column('Amt', 'decimal', format="-zzzz,zz9.999", decimals=3, initial="0", max_width=18, label="Net", column_label="Net", position=5, order=40, help="Total value of events")
        t.column('Qty', 'decimal', format="zzz,zz9-", decimals=2, initial="0", max_width=17, label="Calls", column_label="Calls", position=6, order=50, help="No. of billed calls during this period")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="FirstCall", column_label="FirstCall", position=7, order=60, help="Date of first billed call of this type ")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="LastCall", column_label="LastCall", position=8, order=70, help="Date of last billed call of this type")
        t.column('SlsAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Acct", column_label="Acct", position=9, order=80, help="Account number")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=10, order=90, help="Explanation / memory field for row")
        t.column('FFRow', 'logical', format="Y/N", initial="no", max_width=1, label="Contract", column_label="Contract", position=11, order=100, help="Is this a contract invoice row (Yes/No) ?")
        t.column('Minutes', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Minutes", column_label="Minutes", position=12, order=110, help="Invoice row minutes")
        t.column('FFItemNum', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="ItemNo", column_label="ItemNo", position=13, order=120, help="""Individual """"invisible"""" sequence for this item""")
        t.column('InvRowNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="RowId", column_label="RowId", position=14, order=130, help="Invoice row identification number")
        t.column('PeakMin', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="PeakMin", column_label="PeakMin", position=15, order=140, help="Invoice row peak minutes")
        t.column('OffPeakMin', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="OffPeakMin", column_label="OffPeakMin", position=16, order=150, help="Invoice row offpeak minutes")
        t.column('RowType', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="RowID", column_label="RowID", position=17, order=160, help="Invoice row identification number(Genesys)")
        t.column('InvSect', 'character', format="x(8)", initial="", max_width=16, label="Section Code", column_label="Section", position=19, order=185, help="Code of an Invoice Section")
        t.column('Released', 'logical', format="yes/no", initial="no", help="Events have been released when row was credited", max_width=1, label="Released", column_label="Rel.", position=20, order=190, description='''


''')
        t.column('VATAmt', 'decimal', format="->>>>9.999", decimals=3, initial="0", max_width=18, label="VAT", column_label="VAT", position=22, order=210, help="Amount of V.A.T")
        t.column('VATCode', 'integer', format="z9", initial="1", max_width=4, label="VAT code", column_label="VAT code", position=23, order=205, help="VAT code")
        t.column('VATPerc', 'decimal', format="z9.99", decimals=2, initial="0", max_width=17, label="VAT%", column_label="VAT%", position=24, order=208, help="VAT Percentage (%)")
        t.column('Division', 'character', format="x(8)", initial="", max_width=16, label="Division", column_label="Division", position=25, order=220, help="Division")
        t.column('Department', 'character', format="x(8)", initial="", max_width=16, label="Department", column_label="Department", position=26, order=230, help="Department of Division")
        t.column('CostCentre', 'character', format="x(8)", initial="", max_width=16, label="Ccentre", column_label="Ccentre", position=27, order=240, help="Cost Center of Department")
        t.column('CreditInvNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Credit invoice", column_label="Credit", position=28, order=250, help="Number of credit invoice")
        t.column('Prefix', 'character', format="x(8)", initial="", max_width=16, label="Prefix", column_label="Prefix", position=29, order=180, help="Operator prefix")
        t.column('DataAmt', 'decimal', format="->>>>>>>>>>>>", decimals=2, initial="0", max_width=17, label="Data Amount", column_label="Data", position=30, order=260, help="Transferred data amount")
        t.column('ServiceName', 'character', format="x(15)", initial="", max_width=30, label="Service Name", column_label="Service", position=32, order=310, help="VAS operatorID + keyword")
        t.column('ServiceAddress', 'character', format="x(9)", initial="", max_width=18, label="Service Address", column_label="Serv.Addr", position=33, order=320, help="CGW Short Number (VAS)")
        t.column('VASConAmt', 'decimal', format="->>>>>>9.999", decimals=3, initial="0", max_width=18, label="Consumer Amount", column_label="Cons.Amt", position=34, order=290, help="Amount that has been billed from consumers for VAS tickets")
        t.column('VASOperAmt', 'decimal', format="->>>>>>9.999", decimals=3, initial="0", max_width=18, label="Operator Amount", column_label="OperAmt", position=35, order=300, help="Amount for VAS tickets with operator prices")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="CLI", column_label="CLI", position=36, order=330, help="A-Subscriber number")
        t.column('CreditAmt', 'decimal', format="->>>>>>9.999", decimals=3, initial="0", help="Credited amount", max_width=18, label="Credit Amount", column_label="Credited", position=37, order=340, description='''

''')
        t.column('SubInvNum', 'integer', format=">>9", initial="0", max_width=4, label="SubInvoice Number", column_label="SubInv", position=38, order=350, help="Sequential nbr of the subinvoice within the combined invoice")
        t.column('OrderId', 'integer', format="->>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=39, order=360)
        t.index('InvNum', [['InvNum']], area="Dyn_Index_1", primary=True)

    def down(self):
        self.drop_table('InvRow')
