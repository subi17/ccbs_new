from gearbox.migrations import Migration

class AddTableVASTariff(Migration):

    database = "common"

    def up(self):
        t = self.table('VASTariff', area="Sta_Data_128", dump_name="vastarif")
        t.column('TariffClass', 'character', format="x(2)", initial="", max_width=4, label="TariffClass", column_label="TariffClass", position=2, order=10, help="Tariff Class")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillItem", position=3, order=20, help="Billing item code, max 16 characters")
        t.column('TariffClassName', 'character', format="x(25)", initial="", max_width=50, label="TariffClassName", column_label="Tariff Class Name", position=4, order=30, help="Name of the Tariff Class")
        t.column('PriceIncVAT', 'decimal', format="->>,>>9.999", decimals=3, initial="0", max_width=18, label="PriceIncVat", column_label="PriceIncVat", position=5, order=40, help="Price incl. VAT")
        t.column('PriceExclVAT', 'decimal', format="->>,>>9.999", decimals=3, initial="0", max_width=18, label="PriceExclVAT", column_label="PriceExclVat", position=6, order=50, help="Price Excl. VAT")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Tariff from", column_label="Tariff from", position=7, order=60, help="First effective date")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To Date", column_label="To Date", position=8, order=70, help="Last effective date")
        t.column('CCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="CCN", column_label="CCN", position=9, order=80, help="CCN")
        t.index('TariffClass', [['TariffClass'], ['ToDate', 'DESC']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('VASTariff')
