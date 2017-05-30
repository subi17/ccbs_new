from gearbox.migrations import Migration

class AddFieldFirstBillableSec(Migration):

    database = "common"

    def up(self):
        t = self.alter_table('Tariff')
        t.column('FirstBillableSec', 'integer', format=">>>9", initial="0", label="1st Billable Sec", column_label="1.Sec", position=35, order=320, help="First Billable second")

    def down(self):
        t = self.alter_table('Tariff')
        t.drop_column('FirstBillableSec')
