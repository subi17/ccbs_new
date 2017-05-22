from gearbox.migrations import Migration

class AddFieldCoverageToken(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('OrderCustomer')
        t.column('CoverageToken', 'character', format="X(40)", initial="", label="CoverageToken", column_label="CoverageToken", position=75, order=1140, help="Coverage Token")

    def down(self):
        t = self.alter_table('OrderCustomer')
        t.drop_column('CoverageToken')
