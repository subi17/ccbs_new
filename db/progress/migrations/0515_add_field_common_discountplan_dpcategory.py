from gearbox.migrations import Migration

class AddFieldDPCategory(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('DiscountPlan')
        t.column('DPCategory', 'character', format="x(8)", initial="", max_width=16, label="DPCategory", column_label="DPCategory", position=21, order=200, help="Discount plan category")

    def down(self):
        t = self.alter_table('DiscountPlan')
        t.drop_column('DPCategory')

