from gearbox.migrations import Migration

class AddTableDFTimeTable(Migration):

    database = "common"

    def up(self):
        t = self.table('DFTimeTable', area="Sta_Data_128", label="Dump Time Table", dump_name="dftimetable", desc="Time table for dump files")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('DumpID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Dump ID", column_label="ID", position=3, order=20, help="Unique ID")
        t.column('DumpMode', 'character', format="x(8)", initial="", max_width=16, label="Dump Mode", column_label="Mode", position=6, order=50, help="Dump mode")
        t.column('LastRun', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=17, label="Last Run", column_label="Last", position=7, order=60, help="Latest creation of dump file")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=8, order=70, help="Valid from")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=9, order=80, help="Valid to")
        t.column('DumpDay', 'character', format="x(30)", initial="", max_width=30, label="Days", position=10, order=30, help="Days of the month when file is created")
        t.column('DumpTime', 'character', format="x(20)", initial="", max_width=40, label="Time", position=11, order=40, help="Time when dump files is created")
        t.column('DumpWeekday', 'character', format="x(30)", initial="", max_width=60, label="Weekdays", position=12, order=90, help="Weekdays when dump file is created")
        t.column('FileNameTag', 'character', format="x(20)", initial="", max_width=40, label="File Name Tag", column_label="Tag", position=13, order=100, help="Value for tag used in file name")
        t.column('Ongoing', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Ongoing Run Started", column_label="Ongoing", position=14, order=110, help="Starting time of an ongoing run")
        t.column('UseReplica', 'logical', format="Yes/No", initial="No", max_width=1, label="UseReplica", column_label="UseReplica", position=15, order=120, help="To find Production/Replication")
        t.column('DumpTrigger', 'logical', format="yes/no", initial="no", max_width=1, label="Triggered Dump", column_label="DumpTrigger", position=16, order=130, help="Triggered Dump")
        t.index('DumpID', [['Brand'], ['DumpID'], ['ToDate', 'DESC']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('DFTimeTable')
