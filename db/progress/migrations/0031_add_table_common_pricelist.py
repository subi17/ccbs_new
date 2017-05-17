from gearbox.migrations import Migration

class AddTablePriceList(Migration):

    database = "common"

    def up(self):
        t = self.table('PriceList', area="Sta_Data_128", label="Price Lists", dump_name="pricelis", desc="Price list header")
        t.column('PriceList', 'character', format="x(8)", initial="", max_width=16, label="Plist", column_label="Plist", position=2, order=10, help="Code (identifier) for a Price List")
        t.column('PLName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Name", position=3, order=20, help="Name of a Price List")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=120, label="Memo", column_label="Memo", position=4, order=30, help="Memo")
        t.column('Currency', 'character', format="x(3)", initial="", max_width=6, label="Currency", column_label="Currency", position=5, order=40, help="Price List's currency code")
        t.column('Rounding', 'integer', format="9", initial="2", max_width=4, label="Decimals", column_label="Decimals", position=6, order=50, help="How many decimals will be used for call prices")
        t.column('InclVAT', 'logical', format="Incl/Excl", initial="yes", max_width=1, label="VAT", column_label="VAT", position=7, order=60, help="Is VAT Included/Excluded in tariffs")
        t.column('CurrUnit', 'logical', format="Full/Sub", initial="yes", max_width=1, label="CurrUnit", column_label="CurrUnit", position=8, order=70, help="Currency FULL (1) or SUB (1/100)")
        t.column('AutoCreate', 'character', format="x(30)", initial="", max_width=60, label="Autom.create", column_label="AutoCreate", position=10, order=90, help="Comma separated list of pricelists that follow this pricelist")
        t.column('Prefix', 'character', format="x(5)", initial="", max_width=10, label="Prefix", column_label="Prefix", position=11, order=80, help="Operator prefix where price list is attached to")
        t.column('DedicList', 'logical', format="Dedicated/General", initial="no", max_width=1, label="Dedicated Pricelist", column_label="Dedicated", position=12, order=100, help="Type of price list; general or dedicated to certain customers")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=13, order=110, help="Code Of Brand")
        t.index('PriceList', [['Brand'], ['PriceList']], area="Sta_Index_2", primary=True, unique=True)
        t.index('DedicList', [['Brand'], ['DedicList'], ['PriceList']], area="Sta_Index_2")
        t.index('DedicName', [['Brand'], ['DedicList'], ['PLName']], area="Sta_Index_2")
        t.index('PLName', [['Brand'], ['PLName'], ['PriceList']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('PriceList')
