from gearbox.migrations import Migration

class AddTableCustLetter(Migration):

    database = "common"

    def up(self):
        t = self.table('CustLetter', area="Sta_Data_256", label="Customer Letter", dump_name="custlett", desc="Customer letter text")
        t.column('LtrText', 'character', format="x(78)", initial="", max_width=2686, label="Text", column_label="Text", extent=17, position=2, order=30, help="Customer letter text")
        t.column('ChgDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date", column_label="Date", position=3, order=20, help="Date when text was lastly modified")
        t.column('LtrMargin', 'integer', format=">9", initial="0", max_width=4, label="Margin", column_label="Margin", position=4, order=40, help="Left margin (characters)")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=50, help="Code Of Brand")
        t.index('Brand', [['Brand']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('CustLetter')
