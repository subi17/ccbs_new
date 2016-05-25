from gearbox.migrations import Migration

class AddTableM2MTime(Migration):

    database = "common"

    def up(self):
        t = self.table('M2MTime', area="Sta_Data_256", dump_name="m2mtime")
        t.column('Date', 'date', format="99-99-99", max_width=4, label="Date", column_label="Date", position=2, order=10, help="Date")
        t.column('TimeFrom', 'integer', format=">>>>9", initial="0", max_width=4, label="TimeFrom", column_label="TimeFrom", position=3, order=20, help="Time from")
        t.column('TimeTo', 'integer', format=">>>>9", initial="0", max_width=4, label="TimeTo", column_label="TimeTo", position=4, order=30, help="Time to")
        t.index('Date', [['Date'], ['TimeFrom'], ['TimeTo']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('M2MTime')
