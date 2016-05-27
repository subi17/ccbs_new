from gearbox.migrations import Migration

class AddTableNatHoliday(Migration):

    database = "common"

    def up(self):
        t = self.table('NatHoliday', area="Sta_Data_128", label="National holidays", table_trigger=[{'crc': '?', 'procedure': 'rd-natholiday.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-natholiday.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="natholid", desc="National holidays (weekend prices used)")
        t.column('Holiday', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Date", column_label="Date", position=2, order=10, help="Holiday's date")
        t.column('HName', 'character', format="x(30)", initial="", max_width=60, label="Explan", column_label="Explan", position=3, order=20, help="Holiday's name")
        t.index('Holiday', [['Holiday']], area="Sta_Index_2", primary=True, unique=True)
        t.index('HName', [['HName'], ['Holiday']], area="Sta_Index_2")

    def down(self):
        self.drop_table('NatHoliday')
