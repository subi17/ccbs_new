from gearbox.migrations import Migration

class AddFieldPosId(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('Order')
        t.column('POSid', 'integer', format=">>>>>>9", initial="0", max_width=4, label="POS ID", column_label="POSid", position=107, order=1180, description="ID of the POS when delivery type is POS")

    def down(self):
        t = self.alter_table('Order')
        t.drop_column('POSid')
