from gearbox.migrations import Migration

class AddTableVAService(Migration):

    database = "common"

    def up(self):
        t = self.table('VAService', area="Sta_Data_256", dump_name="VAServic")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="BillCode", column_label="BillCode", position=2, order=10, help="Billing item code, max 16 characters")
        t.column('Bdest', 'character', format="x(8)", initial="", max_width=16, label="Bdest", column_label="Bdest", position=3, order=5, help="B-Destination")
        t.column('ServiceName', 'character', format="x(15)", initial="", max_width=30, label="ServiceName", column_label="Service Name", position=4, order=20, help="OperatorID + keyword")
        t.column('ServiceAddress', 'character', format="x(9)", initial="", max_width=18, label="ServiceAddress", column_label="ServiceAddress", position=5, order=30, help="CGW Short Number")
        t.column('PriceIncVAT', 'decimal', format="->>,>>9.999", decimals=3, initial="0", max_width=18, label="Price Including VAT", column_label="PriceIncVat", position=6, order=40, help="Price incl. VAT")
        t.column('PriceExclVAT', 'decimal', format="->>,>>9.999", decimals=3, initial="0", max_width=18, label="Price Excluding VAT", column_label="PriceExclVAt", position=7, order=50, help="Price Excl. VAT")
        t.column('InvEvent', 'integer', format="9", initial="0", max_width=4, label="InvEvent", column_label="InvEvent", position=8, order=60, help="Invoicable Event")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="From Date", column_label="From", position=9, order=70, help="First effective date")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="To Date", column_label="To", position=10, order=80, help="Last effective date")
        t.column('MinFee', 'decimal', format=">>>9.99", decimals=2, initial="0", max_width=17, label="Minimum Fee", column_label="MinFee", position=11, order=90, help="Minimum fee that is billed from operator each month")
        t.column('ServType', 'character', format="x(2)", initial="", max_width=4, label="ServType", column_label="ServType", position=12, order=100, help="Service Type")
        t.index('BDest', [['Bdest'], ['ServiceName'], ['FromDate', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('ServiceName', [['ServiceName'], ['ServiceAddress']], area="Sta_Index_2")

    def down(self):
        self.drop_table('VAService')
