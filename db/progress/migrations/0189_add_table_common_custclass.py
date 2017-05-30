from gearbox.migrations import Migration

class AddTableCustClass(Migration):

    database = "common"

    def up(self):
        t = self.table('CustClass', area="Sta_Data_256", dump_name="custclas", desc='''
''')
        t.column('CustClass', 'integer', format="9", initial="0", max_width=4, label="Class", column_label="Class", position=2, order=10, help="Customer Class (depend on avg. amount of invoices)")
        t.column('CCName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Name", position=3, order=20, help="Name of the customer class")
        t.column('Amt', 'decimal', format=">>>,>>9.99", decimals=2, initial="0", max_width=17, label="Amt", column_label="Amt", position=4, order=30, help="Lowest limit")
        t.column('Memo', 'character', format="X(60)", initial="", max_width=120, label="Memo", column_label="Memo", position=6, order=50, help="Memo")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=7, order=60, help="Code Of Brand")
        t.index('Class', [['Brand'], ['CustClass'], ['Amt']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Amt', [['Brand'], ['Amt']], area="Sta_Index_2")
        t.index('CCName', [['Brand'], ['CCName'], ['CustClass']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CustClass')
