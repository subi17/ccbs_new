from gearbox.migrations import Migration

class AddTableBankAccount(Migration):

    database = "common"

    def up(self):
        t = self.table('BankAccount', area="Sta_Data_64", label="Company's Bank Accounts", dump_name="bankacco", desc="Company's bank accounts")
        t.column('UnitCode', 'integer', format=">>9", initial="0", max_width=4, label="Unit", position=2, order=10, help="Unit number")
        t.column('BankAccount', 'character', format="X(20)", initial="", max_width=40, label="Bank Account", position=3, order=20, help="Bank Account")
        t.column('BankData', 'character', format="X(20)", initial="", max_width=40, label="Bank Data", position=4, order=30, help="Bank account in standard format for transfer files")
        t.column('PrintCode', 'character', format="X(8)", initial="", help="Printing to forms", max_width=16, label="Printing", position=5, order=40, description="One position per form, e.g. 1 = invoice, 2 = reminder. Empty or zero means that account is not printed, a positive value indicates the printing order if several accounts are printed. ")
        t.column('Currency', 'character', format="x(5)", initial="", max_width=10, label="Currency", column_label="Currency", position=6, order=50, help="Currency code")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=7, order=60, help="Code Of Brand")
        t.column('InvForm', 'character', format="x(60)", initial="", max_width=120, label="Invoice Forms", column_label="Forms", position=8, order=70, help="Invoice forms, to which this bank account is printed")
        t.column('BarCode', 'character', format="x(60)", initial="", max_width=120, label="BarCode Forms", column_label="BarCode", position=9, order=80, help="Invoice forms, in which bar code uses this bank account")
        t.column('BankOffice', 'character', mandatory=True, format="x(40)", initial="", max_width=80, label="Office", column_label="Office", position=10, order=90, help="Bank office's name")
        t.column('CreditorId', 'character', format="X(35)", initial="", max_width=70, label="Creditor Id", column_label="CreditorId", position=11, order=100, help="Creditor ID for CSB19.14")
        t.column('BIC', 'character', format="X(11)", initial="", max_width=22, label="BIC", column_label="BIC", position=12, order=110, help="BIC code")
        t.column('BankCodes', 'character', format="x(30)", initial="", max_width=60, label="BankCodes", column_label="BankCodes", position=13, order=120, help="Bank codes")
        t.column('DDAllocation', 'decimal', format="Z9.99", decimals=2, initial="0", max_width=17, label="DDAllocation", column_label="DDAllocation", position=14, order=130, help="Direct debit allocation value")
        t.index('UnitBank', [['Brand'], ['UnitCode'], ['BankAccount']], area="Sta_Index_2", primary=True, unique=True)
        t.index('AccNum', [['Brand'], ['BankAccount']], area="Sta_Index_2")

    def down(self):
        self.drop_table('BankAccount')
