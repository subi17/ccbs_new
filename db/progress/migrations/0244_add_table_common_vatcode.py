from gearbox.migrations import Migration

class AddTableVATCode(Migration):

    database = "common"

    def up(self):
        t = self.table('VATCode', area="Sta_Data_128", label="VAT Code", dump_name="vatcode", desc="VAT Code")
        t.column('VATCode', 'integer', valexp="vc-code < 11", format="z9", initial="0", max_width=4, label="VAT code", column_label="VAT code", position=2, order=10, valmsg="VAT code must be between 1 ... 10 !", help="VAT code")
        t.column('VCName', 'character', format="x(40)", initial="", max_width=80, label="Explanation", column_label="Explanation", position=3, order=20, help="Explanation for this VAT code")
        t.column('VATPerc', 'decimal', format="z9.99", decimals=2, initial="0", max_width=17, label="%", column_label="%", position=4, order=30, help="Amount of VAT (%)")
        t.column('AccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Number", column_label="Number", position=5, order=40, help="Account number")
        t.column('TaxZone', 'character', format="x(8)", initial="", max_width=16, label="Tax Zone", column_label="Zone", position=7, order=50, help="Tax Zone")
        t.column('TaxClass', 'character', format="x(8)", initial="", help="Tax class", max_width=16, label="Tax Class", column_label="Class", position=8, order=60, description='''

''')
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="Valid To", column_label="To", position=9, order=80, help="Date when VAT expires")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="From", position=10, order=70, help="Date when VAT becomes effective")
        t.index('VatCode', [['VATCode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('TaxClass', [['TaxClass'], ['TaxZone'], ['ToDate', 'DESC']], area="Sta_Index_2")
        t.index('TaxZone', [['TaxZone'], ['TaxClass'], ['ToDate', 'DESC']], area="Sta_Index_2")
        t.index('VatPerc', [['VATPerc'], ['VATCode'], ['ToDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('VATCode')
