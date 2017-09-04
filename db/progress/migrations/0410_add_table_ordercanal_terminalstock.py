from gearbox.migrations import Migration

class AddTableTerminalStock(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('TerminalStock', area="Sta_Data_64", dump_name="terminal")
        t.column('BillCode', 'character', format="x(8)", initial="", max_width=16, label="BillCode", column_label="BillCode", position=2, order=10)
        t.column('Amount', 'integer', format="->>>,>>9", initial="0", max_width=4, label="Amount", column_label="Amount", position=3, order=20)
        t.column('Description', 'character', format="x(20)", initial="", max_width=40, label="Description", column_label="Description", position=4, order=30)
        t.column('Colour', 'character', format="x(8)", initial="", max_width=16, label="Colour", column_label="Colour", position=5, order=40)
        t.index('BillCode', [['BillCode']], area="Sta_Index_1", primary=True)

    def down(self):
        self.drop_table('TerminalStock')
