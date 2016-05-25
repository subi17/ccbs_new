from gearbox.migrations import Migration

class AddTableWInvPwd(Migration):

    database = "common"

    def up(self):
        t = self.table('WInvPwd', area="Sta_Data_256", label="Web Password", dump_name="winvpwd", desc="Password for Web Invoice")
        t.column('CustNum', 'integer', mandatory=True, format="ZZZZZZZZ9", initial="0", max_width=4, label="CustNo", column_label="Cust.no", position=2, order=10, help="Customer's number")
        t.column('PwdSeries', 'integer', format=">>>>9", initial="0", max_width=4, label="Series", column_label="Series", position=3, order=20, help="Password list serial number")
        t.column('Passwd', 'integer', format="99999", initial="0", max_width=1200, label="Password", column_label="Password", extent=100, position=4, order=30, help="THE Password")
        t.column('ValidFrom', 'date', format="99-99-99", max_width=4, label="ValidFrom", column_label="ValidFrom", position=5, order=40, help="Valid from date")
        t.column('Used', 'integer', format=">>9", initial="0", max_width=4, label="Used", column_label="Used", position=6, order=50, help="How many passwords are used")
        t.column('LoginId', 'character', format="X(16)", initial="", max_width=32, label="LoginId", column_label="Login", position=7, order=60, help="Login id for the customer")
        t.column('CSPasswd', 'character', format="X(20)", initial="", max_width=4200, label="Password", column_label="Passwd", extent=100, position=8, case_sensitive=True, order=70, help="A case-sensitive password")
        t.index('CustNum', [['CustNum'], ['PwdSeries'], ['Used']], area="Sta_Index_2", primary=True, unique=True)
        t.index('LoginId', [['LoginId']], area="Sta_Index_2")

    def down(self):
        self.drop_table('WInvPwd')
