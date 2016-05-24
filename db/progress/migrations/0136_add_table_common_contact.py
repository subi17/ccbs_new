from gearbox.migrations import Migration

class AddTableContact(Migration):

    database = "common"

    def up(self):
        t = self.table('Contact', area="Sta_Data_256", label="Contact", dump_name="contact", desc='''Customer contact calendar and history


''')
        t.column('CustNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Cust", position=2, order=10, help="Customer number")
        t.column('UserCode', 'character', format="x(8)", initial="", max_width=16, label="User Code", column_label="User", position=3, order=20, help="User who handled this contact")
        t.column('ConDate', 'date', format="99-99-99", max_width=4, label="Contact Date", column_label="Date", position=4, order=30, help="Date when contact was taken")
        t.column('ConStamp', 'decimal', format="99999999.99999", decimals=2, initial="0", max_width=17, label="Contact Taken", column_label="Taken", position=5, order=40, help="Time stamp when contact was taken")
        t.column('ConState', 'integer', format="9", initial="0", max_width=4, label="Status", column_label="Status", position=6, order=50, help="Status of contact event")
        t.column('CustBal', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Customer's Balance", column_label="Balance", position=7, order=60, help="Customer's open balance")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=8, order=70, help="Code Of Brand")
        t.column('PlanDate', 'date', format="99-99-99", max_width=4, label="Contact Planned", column_label="Planned", position=9, order=80, help="Date when contact was planned to be taken")
        t.column('ConType', 'integer', format="9", initial="0", max_width=4, label="Contact Type", column_label="Type", position=10, order=90, help="Type of contact")
        t.column('ConID', 'integer', format=">>>>>>>>9", initial="0", help="Unique contact ID", max_width=4, label="Contact ID", column_label="ID", position=11, order=100, description="Sequence ConID")
        t.index('CustNum', [['Brand'], ['CustNum'], ['ConStamp', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('ConID', [['ConID']], area="Sta_Index_2", unique=True)
        t.index('ConStamp', [['Brand'], ['ConStamp', 'DESC']], area="Sta_Index_2")
        t.index('ConType', [['Brand'], ['ConType'], ['ConState']], area="Sta_Index_2")
        t.index('UserCode', [['Brand'], ['UserCode'], ['ConState'], ['ConDate', 'DESC'], ['CustNum']], area="Sta_Index_2")
        t.index('UserStamp', [['Brand'], ['UserCode'], ['ConStamp', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Contact')
