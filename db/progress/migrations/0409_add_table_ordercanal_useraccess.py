from gearbox.migrations import Migration

class AddTableUserAccess(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('UserAccess', area="Sta_Data_256", dump_name="UserAcce", desc="Which customer has what role for a subscription")
        t.column('CustNum', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Customer", position=2, order=10, help="Customer")
        t.column('Role', 'character', format="x(6)", initial="", max_width=12, label="Role", position=3, order=20, help="The access type of the customer on the subscription")
        t.column('CLI', 'character', format="x(20)", initial="", max_width=40, label="CLI", position=4, order=30, help="The subscription, to which the user has access")
        t.column('SubType', 'character', format="X(9)", initial="", max_width=18, label="Type", position=5, order=40, help="Type of the subscription, e.g. mobile or broadband")
        t.index('customer', [['CustNum'], ['CLI']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('UserAccess')
