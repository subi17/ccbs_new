from gearbox.migrations import Migration

class AddTableCustCat(Migration):

    database = "common"

    def up(self):
        t = self.table('CustCat', area="Sta_Data_128", label="Customer Category", dump_name="custcat", desc="Customer category")
        t.column('Category', 'character', format="x(4)", initial="", max_width=8, label="Category", column_label="Category", position=2, order=10, help="Category code, max 4 characters")
        t.column('CatName', 'character', format="x(30)", initial="", max_width=60, label="CategName", column_label="CategName", position=3, order=20, help="Name's of customer category")
        t.column('MaxCredit', 'integer', format="z,zzz,zz9", initial="0", max_width=4, label="MaxCredit", column_label="MaxCredit", position=4, order=30, help="Max credit for a customer in this category")
        t.column('ArAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Receivables Account", column_label="Receiv.", position=5, order=40, help="Account no. for Receivables")
        t.column('PerAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Periodizing Account", column_label="Period.", position=6, order=50, help="Account for periodizing")
        t.column('UnbillAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Unbilled Account", column_label="Unbilled", position=7, order=60, help="Account no. for unbilled events (balance sheet)")
        t.column('IntType', 'integer', format="9", initial="1", max_width=4, label="Interest type", column_label="IntType", position=8, order=70, help="1 = fixed interest, 2 = in addition to euribor")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=9, order=80, help="Code Of Brand")
        t.column('PaymTerm', 'integer', format=">9", initial="0", max_width=4, label="Payment Term", column_label="PaymTerm", position=10, order=90, help="Terms of payment (days)")
        t.column('CustIdType', 'character', format="x(8)", initial="", help="Customer ID type", max_width=16, label="Customer ID Type", column_label="ID Type", position=11, order=100, description='''

''')
        t.column('SelfEmployed', 'logical', format="yes/no", initial="no", max_width=1, label="Selfemployed", column_label="Selfempl.", position=12, order=110)
        t.column('MobSubLimit', 'integer', format=">>>>9", initial="0", max_width=4, label="Subscription Max Limit", column_label="MSLimit", position=13, order=120)
        t.column('ActivationLimit', 'integer', format=">>>>9", initial="0", max_width=4, label="Activation Limit", column_label="ActivationLimit", position=14, order=130, description="Activation limit per customer")
        t.index('Category', [['Brand'], ['Category']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CatName', [['Brand'], ['CatName'], ['MaxCredit']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CustCat')
