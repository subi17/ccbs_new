from gearbox.migrations import Migration

class AddTableUserAccount(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('UserAccount', area="Sta_Data_256", dump_name="useracco", desc="Enduser accounts for logging-in on selfcare pages")
        t.column('login', 'character', format="X(12)", initial="", max_width=24, label="Login", position=2, order=10, help="Login name")
        t.column('CustNum', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Customer", position=3, order=20, help="The customer that can use this account")
        t.column('password', 'character', format="X(20)", initial="", max_width=40, label="Passw", position=4, order=30, help="Password")
        t.column('active', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="status", position=5, order=40, help="Active status")
        t.index('login', [['login']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustNum', [['CustNum']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('UserAccount')
