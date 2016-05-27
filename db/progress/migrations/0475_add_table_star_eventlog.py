from gearbox.migrations import Migration

class AddTableeventlog(Migration):

    database = "star"

    def up(self):
        t = self.table('eventlog', area="Sta_Data_64", dump_name="eventlog")
        t.column('eventdate', 'date', format="99.99.99", initial=self.unknown, max_width=4, label="Date", position=2, order=10, help="Event date")
        t.column('eventtime', 'character', format="X(8)", initial="", max_width=16, label="Time", position=3, order=20, help="Event time")
        t.column('usercode', 'character', format="X(8)", initial="", max_width=16, label="User", position=4, order=30, help="User code")
        t.column('tablename', 'character', format="X(15)", initial="", max_width=30, label="Table", position=5, order=40, help="Source table name")
        t.column('key', 'character', format="X(30)", initial="", max_width=60, label="Key", position=6, order=50, help="Key to source record")
        t.column('action', 'character', format="X(8)", initial="", max_width=16, label="Action", position=7, order=60, help="Action type.. Create,Modity,Delete")
        t.column('datavalues', 'character', format="X(50)", initial="", max_width=100, label="Values", position=8, order=70, help="Changed values of record")
        t.column('modifiedfields', 'character', format="X(50)", initial="", max_width=100, label="Modified fields", position=9, order=80, help="Fieldnames from BUFFER-COMPARE")
        t.column('fieldformats', 'character', format="X(50)", initial="", max_width=100, label="Field formats", position=10, order=90, help="Field formats of modified fields")
        t.column('EventLogStatus', 'integer', format="9", initial="0", max_width=4, label="EventLogStatus", column_label="EventLogStatus", position=11, order=100, help="Status of eventlog")
        t.column('TimingTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="TimingTS", column_label="TimingTS", position=12, order=110, help="Timing Timestamp")
        t.column('TimingDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="TimingDate", column_label="TimingDate", position=13, order=120, help="Timing Date (schedule date)")
        t.column('TimingTime', 'decimal', format="99.99", decimals=2, initial="0", max_width=17, label="TimingTime", column_label="TimingTime", position=14, order=130, help="Timing Time (Scheluded time)")
        t.column('Memo', 'character', format="x(40)", initial="", max_width=80, label="Memo", column_label="Memo", position=15, order=140)
        t.index('TableName', [['tablename'], ['key'], ['eventdate', 'DESC'], ['eventtime', 'DESC']], area="Sta_Index_1", primary=True)
        t.index('EventDate', [['eventdate', 'DESC'], ['eventtime', 'DESC'], ['tablename'], ['key']], area="Sta_Index_1")
        t.index('EventLogStatus', [['EventLogStatus'], ['TimingTS', 'DESC']], area="Sta_Index_1")
        t.index('UserName', [['usercode'], ['eventdate', 'DESC'], ['eventtime', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('eventlog')
