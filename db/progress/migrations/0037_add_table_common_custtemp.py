from gearbox.migrations import Migration

class AddTableCustTemp(Migration):

    database = "common"

    def up(self):
        t = self.table('CustTemp', area="Sta_Data_128", label="Customer Templates", dump_name="custemp", desc='''Template definition for Customer creation
''')
        t.column('TemplNum', 'integer', format=">9", initial="0", max_width=4, label="Templ", column_label="Templ", position=2, order=10, help="Template Number (1 .. 99)")
        t.column('TemplName', 'character', format="x(40)", initial="", max_width=80, label="Description", column_label="Description", position=3, order=20, help="Description Of Template")
        t.column('CustNum', 'integer', mandatory=True, format="zzzzzz9", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=4, order=30, help="Number of Template Customer")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=1830, label="Memo", column_label="Memo", extent=15, position=5, order=40, help="Memo text")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('TemplNum', [['Brand'], ['TemplNum']], area="Sta_Index_2", primary=True, unique=True)
        t.index('TemplName', [['Brand'], ['TemplName'], ['TemplNum']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CustTemp')
