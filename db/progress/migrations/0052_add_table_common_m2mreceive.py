from gearbox.migrations import Migration

class AddTableM2MReceive(Migration):

    database = "common"

    def up(self):
        t = self.table('M2MReceive', area="Sta_Data_32", dump_name="m2mrecei")
        t.column('RecStatus', 'integer', format=">9", initial="0", max_width=4, label="Status", column_label="Status", position=2, order=140)
        t.column('XMLMessage', 'character', format="x(8)", initial="", max_width=16, label="XML Message", column_label="XML Message", position=3, order=100)
        t.index('RecStatus', [['RecStatus']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('M2MReceive')
