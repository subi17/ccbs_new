from gearbox.migrations import Migration

class AddFieldFixedBundle(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('Offer')
        t.column('Offer_type', 'character', format="X(12)", initial="", max_width=60, label="Offer type", column_label="Offer_type", position=17, order=170, help="Offer type")

    def down(self):
        t = self.alter_table('Offer')
        t.drop_column('Offer_type')
