from gearbox.migrations import Migration

class AddFieldItemType(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('BillItem')
        t.column('ItemType', 'integer', format=">>9", initial="0", max_width=4, label="Item Type", position=26, order=390, help="Type 0 for mobile 1 for convergent")

    def down(self):
        t = self.alter_table('BillItem')    
        t.drop_column('ItemType')
