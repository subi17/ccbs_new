from gearbox.migrations import Migration

class AddFieldEstimatedDataSpeed(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('OrderFusion')
        t.column('EstimatedDataSpeed', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="EstDataSpeed", column_label="EstDataSpeed", position=28, order=280, description="Estimated Data Speed")

    def down(self):
        t = self.alter_table('OrderFusion')
        t.drop_column('EstimatedDataSpeed')
