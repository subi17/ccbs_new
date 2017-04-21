from gearbox.migrations import Migration

class AddFieldRateBDest(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('BankAccount')
        t.column('PresenterId', 'character', format="X(35)", initial="", label="Presenter Id", column_label="PresenterId", position=15, order=140, help="Presenter ID for CSB19.14")

    def down(self):
        t = self.alter_table('BDest')
        t.drop_column('RateBDest')
