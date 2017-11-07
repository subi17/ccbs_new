from gearbox.migrations import Migration

class AddTableDDAuth(Migration):

    database = "common"

    def up(self):
        t = self.table('DDAuth', area="Sta_Data_64", label="Direct Debit Authorization", dump_name="ddauth", desc="Direct debit authorization")
        t.column('CustNum', 'integer', format="zzzzzzz", initial="0", max_width=4, label="Customer Nbr", column_label="Cust.Nbr", position=2, order=10, help="Billable customer number")
        t.column('CustName', 'character', format="x(35)", initial="", max_width=70, label="Billable", column_label="Billable", position=3, order=20, help="Customer's name (in authorization)")
        t.column('DDProcess', 'integer', format="9", initial="0", max_width=4, label="Processing Code", column_label="Processing Code", position=4, order=30, help="Processing Code: 1=new, 2=change, 3=termination, 4=maintenance")
        t.column('Archive', 'character', format="x(20)", initial="", max_width=40, label="Archive Code", column_label="Archive Code", position=5, order=40, help="Archive code of authorization")
        t.column('BankAcc', 'character', format="x(30)", initial="", max_width=60, label="Bank Account", column_label="Bank Account", position=6, order=50, help="Payer's current bank account")
        t.column('AuthDate', 'date', format="9999/99/99", initial=self.unknown, max_width=4, label="Event Date", column_label="Event Date", position=7, order=60, help="Event date of authorization")
        t.column('OldBankAcc', 'character', format="x(30)", initial="", max_width=60, label="Old Bank Account", column_label="Old Bank Account", position=8, order=70, help="Payer's old bank account")
        t.column('Identification', 'character', format="x(30)", initial="", max_width=60, label="Identification", column_label="Identification", position=9, order=80, help="Identification (cust.nbr, name, telephone etc.)")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=10, order=90, help="Code Of Brand")
        t.column('AuthID', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Authorization ID", column_label="ID", position=11, order=100, help="Unique ID")
        t.index('CustNum_s', [['CustNum'], ['AuthDate']], area="Sta_Index_2", primary=True)
        t.index('AuthDate', [['Brand'], ['AuthDate', 'DESC'], ['CustNum']], area="Sta_Index_2")
        t.index('CustNum', [['Brand'], ['CustNum'], ['AuthDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('DDAuth')
