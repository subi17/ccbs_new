from gearbox.migrations import Migration

class AddFieldPort(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('OrderFusion')
        t.column('portStat', 'character', format="x(15)", initial="", max_width=30, label="portStat", column_label="portStat", position=29, order=280, help="Portability Status")
        t.column('portDate', 'character', format="x(20)", initial="", max_width=40, label="portDate", column_label="portDate", position=30, order=290, help="Portability Date")
        t.column('routerStat', 'character', format="x(10)", initial="", max_width=20, label="routerStat", column_label="routerStat", position=31, order=300, help="Router Status")

    def down(self):
        t = self.alter_table('OrderFusion')
        t.drop_column('portStat')
        t.drop_column('portDate')
        t.drop_column('routerStat')
