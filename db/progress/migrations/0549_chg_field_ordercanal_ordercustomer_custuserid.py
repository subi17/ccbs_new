from gearbox.migrations import Migration

class ChgFieldCustUserIdCustPWD(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('OrderCustomer')
        t.rename_column('CustUserId', 'AuthCustId')
        t.rename_column('CustPWD', 'AuthCustIdType')
        t.alter_column('AuthCustId', format="x(11)", label="AuthCustId", column_label="AuthCustId", help="Authorization Customer ID")
        t.alter_column('AuthCustIdType', label="AuthCustIdType", column_label="AuthCustIdType", help="Authorization Customer ID Type")

    def down(self):
        t = self.alter_table('OrderCustomer')
        t.rename_column('AuthCustId', 'CustUserId')
        t.rename_column('AuthCustIdType', 'CustPWD')
        t.alter_column('CustUserId', format="x(8)", label="UserId", column_label="UserId", help="")
        t.alter_column('CustPWD', label="Password", column_label="Password", help="")
