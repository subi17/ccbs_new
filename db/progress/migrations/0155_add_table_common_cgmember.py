from gearbox.migrations import Migration

class AddTableCGMember(Migration):

    database = "common"

    def up(self):
        t = self.table('CGMember', area="Sta_Data_256", label="Customer Group Members", dump_name="cgmember", desc="Customer Group Member records;  join customers&cust.groups")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer's number")
        t.column('CustGroup', 'character', format="x(10)", initial="", max_width=20, label="Customer Group", column_label="Customer Group", position=3, order=5, help="Individual Code for a Customer Group")
        t.column('CustName', 'character', format="x(30)", initial="", max_width=60, label="Customer's name", column_label="Customer's name", position=4, order=20, help="Customer's name")
        t.column('Memo', 'character', format="x(10)", initial="", max_width=20, label="Info", column_label="Info", position=5, order=30, help="Additional coded information about membership")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=40, help="Code Of Brand")
        t.index('CustGroup', [['Brand'], ['CustGroup'], ['CustNum']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustName', [['Brand'], ['CustGroup'], ['CustName'], ['CustNum']], area="Sta_Index_2", unique=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['CustGroup']], area="Sta_Index_2", unique=True)
        t.index('CustNum_s', [['CustNum'], ['CustGroup']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('CGMember')
