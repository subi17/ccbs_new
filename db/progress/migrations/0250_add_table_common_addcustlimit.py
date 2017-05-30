from gearbox.migrations import Migration

class AddTableAddCustLimit(Migration):

    database = "common"

    def up(self):
        t = self.table('AddCustLimit', area="Sta_Data_256", dump_name="addcustl", desc="Increase customer's credit limit temporarily")
        t.column('CustNum', 'integer', format="zzzzzzzzz", initial="0", max_width=4, label="Customer", column_label="Cust", position=2, order=10, help="Customer Number")
        t.column('dTo', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="DateTo", column_label="DateTo", position=3, order=20, help="Last extra limit day")
        t.column('Amount', 'decimal', format="z,zz9.99", decimals=2, initial="0", max_width=17, label="Amount", position=4, order=30, help="Amount of Extra Customer limit")
        t.column('Memo', 'character', format="X(50)", initial="", max_width=100, label="Memo", column_label="Memo", position=5, order=40, help="Reason for Extra Customer Limit")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('CustNum_s', [['CustNum'], ['dTo', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['dTo', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('AddCustLimit')
