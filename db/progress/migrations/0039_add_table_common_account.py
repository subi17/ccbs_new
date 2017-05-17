from gearbox.migrations import Migration

class AddTableAccount(Migration):

    database = "common"

    def up(self):
        t = self.table('Account', area="Sta_Data_128", label="Accounts", dump_name="account", desc="Accounts")
        t.column('AccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Account", column_label="Account", position=2, order=10, help="Account number")
        t.column('AccName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Name", position=3, order=20, help="Account name")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=4, order=30, help="Memo for account")
        t.column('VATCode', 'integer', format="z9", initial="0", max_width=4, label="VAT Code", column_label="VAT Code", position=5, order=40, help="VAT code")
        t.column('AccType', 'integer', format=">9", initial="0", max_width=4, label="Type", column_label="Type", position=6, order=50, help="Account usage type number")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=7, order=60, help="Code Of Brand")
        t.index('AccNum', [['Brand'], ['AccNum']], area="Sta_Index_2", primary=True, unique=True)
        t.index('AccName', [['Brand'], ['AccName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Account')
