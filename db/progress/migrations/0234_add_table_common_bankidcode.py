from gearbox.migrations import Migration

class AddTableBankIdCode(Migration):

    database = "common"

    def up(self):
        t = self.table('BankIdCode', area="Sta_Data_64", label="BankIdCodes", dump_name="BankIdCode", desc="Bank identifier Code")
        t.column('BIC', 'character', format="X(11)", initial="", max_width=22, label="BIC", column_label="BIC", position=2, order=10, help="Bank Identifier Code")
        t.column('BankCode', 'character', format="X(4)", initial="", max_width=8, label="BankCode", column_label="BankCode", position=3, order=20, help="Bank Code")
        t.column('BankName', 'character', format="X(60)", initial="", max_width=120, label="BankName", column_label="BankName", position=4, order=30, help="Bank Name")
        t.column('BankType', 'character', format="x(10)", initial="", max_width=20, label="Bank Type", column_label="BankType", position=5, order=40, help="Bank Type (ie: IberPay or EBA)")
        t.index('BCode', [['BankCode']], area="Dyn_Index_1", primary=True, unique=True)

    def down(self):
        self.drop_table('BankIdCode')
