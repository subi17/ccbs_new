from gearbox.migrations import Migration

class AddTableFusionInvoice(Migration):

    database = "common"

    def up(self):
        t = self.table('FusionInvoice', area="Dyn_Data_32", label="FusionInvoice", dump_name="fusioninvoice", desc="FusionInvoice")
        t.column('FuInvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="FuInvNo", column_label="FuInvNo", position=2, order=10, help="Consecutive Fusion Invoice Number, 1 ... 99999999")
        t.column('InvDate', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="InvDate", column_label="InvDate", position=3, order=20, help="Invoice date")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=4, order=30, help="Mobile customer number")
        t.column('MsSeq', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="MsSeq", column_label="MsSeq", position=5, order=40, help="Link to mobsub-table")
        t.column('InvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="InvNo", column_label="InvNo", position=6, order=50, help="Mobile Invoice Number")
        t.column('Mapping', 'integer', format="9", initial="0", max_width=4, label="Mapping", column_label="Mapping", position=7, order=60, help="1=Mobile+Fixed, 2=Mobile, 3=Fixed")
        t.column('DeliveryState', 'integer', format=">9", initial="0", max_width=4, label="Delivery State", column_label="DeliveryState", position=8, order=70)
        t.column('MTariffMF', 'decimal', format="-zzz,zz9.99", decimals=3, initial="0", max_width=18, label="MTariffMF", column_label="MTariffMF", position=9, order=80, help="Mobile tariff monthly fee")
        t.column('MOtherMF', 'decimal', format="-zzz,zz9.99", decimals=3, initial="0", help="Other mobile monthly fee as Spotify, Data bundles, etc...", max_width=18, label="MOtherMF", column_label="MOtherMF", position=10, order=90, description="Total, details in VatAmount")
        t.column('MTraffic', 'decimal', format="-zzz,zz9.99", decimals=3, initial="0", max_width=18, label="MTraffic", column_label="MTraffic", position=11, order=100, help="Mobile traffic amount")
        t.column('MTaxableIncome', 'decimal', format="-zzz,zz9.99", decimals=3, initial="0", max_width=18, label="MTaxableIncome", column_label="MTaxableIncome", position=12, order=110, help="Mobile taxable income")
        t.column('MVatAmt', 'decimal', format="-zzz,zz9.99", decimals=3, initial="0", help="Amount of VAT", max_width=18, label="VAT", column_label="VAT", position=13, order=120, description="Total, details in VatAmount")
        t.column('MTotalInvoice', 'decimal', format="-zzz,zz9.99", decimals=3, initial="0", max_width=18, label="To pay", column_label="To pay", position=14, order=130, help="Total payable")
        t.column('MTermFinancing', 'decimal', format="-zzz,zz9.99", decimals=3, initial="0", max_width=18, label="To pay", column_label="To pay", position=15, order=140, help="Total payable")
        t.column('MInvAmt', 'decimal', format="-zzz,zz9.99", decimals=3, initial="0", max_width=18, label="To pay", column_label="To pay", position=16, order=150, help="Total payable")
        t.column('CustomerId', 'character', format="x(11)", initial="", max_width=22, label="CustomerId", column_label="CustomerId", position=17, order=160, help="Fixed line customer id")
        t.column('InvoiceNum', 'character', format="x(14)", initial="", max_width=28, label="InvoiceNum", column_label="InvoiceNum", position=18, order=170, help="Fixed line invoice number")
        t.column('FixedNumber', 'character', format="x(14)", initial="", max_width=28, label="Fixed Number", column_label="FixedNumber", position=19, order=180, help="Fixed line number")
        t.column('Language', 'character', format="x(2)", initial="", max_width=4, label="Language", column_label="Language", position=20, order=190)
        t.column('Email', 'character', format="x(60)", initial="", max_width=120, label="Email", column_label="Email", position=21, order=200, help="Customer's Email Address")
        t.column('ProductCode', 'character', format="x(14)", initial="", max_width=28, label="Product Code", column_label="ProductCode", position=22, order=210, help="Base offer product code")
        t.column('ProductText', 'character', format="x(40)", initial="", max_width=80, label="Product Text", column_label="ProductText", position=23, order=220, help="Base offer product text")
        t.column('MSubsType', 'character', format="x(10)", initial="", max_width=20, label="MSubsType", column_label="MSubsType", position=24, order=230, help="Mobile subscription tariff")
        t.column('BaseServiceMF', 'decimal', format="-zzz,zz9.99", decimals=4, initial="0", max_width=19, label="Base Service MF", column_label="BaseServiceMF", position=25, order=240, help="Monthly fee base service")
        t.column('OtherMF', 'decimal', format="-zzz,zz9.99", decimals=4, initial="0", max_width=19, label="Other MF", column_label="OtherMF", position=26, order=250, help="Other monthly fee")
        t.column('TrafficAmt', 'decimal', format="-zzz,zz9.99", decimals=4, initial="0", max_width=19, label="Traffic amount", column_label="TrafficAmt", position=27, order=260, help="Traffic amount")
        t.column('OtherItems', 'decimal', format="-zzz,zz9.99", decimals=4, initial="0", max_width=19, label="Other items", column_label="OtherItems", position=28, order=270, help="Other items")
        t.column('TaxIncome', 'decimal', format="-zzz,zz9.99", decimals=4, initial="0", max_width=19, label="Taxable income", column_label="TaxIncome", position=29, order=280, help="Taxable income")
        t.column('TaxAmt', 'decimal', format="-zzz,zz9.99", decimals=4, initial="0", max_width=19, label="Tax Amount", column_label="TaxAmt", position=30, order=290, help="Tax Amount")
        t.column('TotalAmt', 'decimal', format="-zzz,zz9.99", decimals=3, initial="0", max_width=18, label="Total Amount", column_label="TotalAmt", position=31, order=300, help="Total invoice amount")
        t.column('AddItems', 'decimal', format="-zzz,zz9.99", decimals=4, initial="0", max_width=19, label="Additional items", column_label="AddItems", position=32, order=310, help="Amounts outside the invoice but to be charged with the invoice")
        t.column('TotalToPay', 'decimal', format="-zzz,zz9.99", decimals=3, initial="0", max_width=18, label="Total to pay", column_label="TotalToPay", position=33, order=320)
        t.column('MPermPenalty', 'decimal', format="-zzz,zz9.99", initial="0", max_width=15, label="MPermPenalty", column_label="MPermPenalty", position=34, order=330, help="Mobile Permanency Penalty Fee")
        t.index('FuInvNum', [['FuInvNum']], area="Dyn_Index_1", primary=True, unique=True)
        t.index('CustNum', [['CustNum'], ['InvDate', 'DESC']], area="Dyn_Index_1")
        t.index('CustomerID', [['CustomerId'], ['InvDate', 'DESC']], area="Dyn_Index_1")
        t.index('InvDate', [['InvDate', 'DESC']], area="Dyn_Index_1")
        t.index('InvNum', [['InvNum']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('FusionInvoice')
