from gearbox.migrations import Migration

class AddTableDumpLog(Migration):

    database = "common"

    def up(self):
        t = self.table('DumpLog', area="Sta_Data_64", label="Dump Log", dump_name="DumpLog", desc="Collect all dump file status information")
        t.column('DumpLogId', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Dump Log Id", position=2, order=5, help="Unique ID for Dump Log")
        t.column('DumpId', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Dump Id", position=3, order=10, help="Dump Id")
        t.column('FileName', 'character', format="x(50)", initial="", max_width=100, label="File Name", position=4, order=20, help="Name of dump file")
        t.column('DumpType', 'character', format="x(8)", initial="", max_width=16, label="Dump mode", position=5, order=30, help="Whether type is full or incremental")
        t.column('CreateStart', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Creation start time", position=6, order=40, help="Start time of dump creation")
        t.column('CreateEnd', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Creation end time", position=7, order=50, help="End time of dump creation")
        t.column('Filesize', 'decimal', format=">>>>>>>>>>9", decimals=0, initial="0", max_width=15, label="File size", position=8, order=60, help="Size of dump file")
        t.column('CompFilesize', 'decimal', format=">>>>>>>>>>9", decimals=0, initial="0", max_width=15, label="Compress File size", position=9, order=70, help="Size of compressed dump file")
        t.column('TransStart', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Transfer start time", position=10, order=80, help="Start time of dump transfer")
        t.column('TransEnd', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Transfer end time", position=11, order=90, help="End time of dump transfer")
        t.column('DumpLogStatus', 'integer', format=">9", initial="0", max_width=4, label="Dump Log Status", position=12, order=100, help="Status of current state of DumpLog")
        t.column('CreationDB', 'character', format="X(8)", initial="", max_width=16, label="CreationDB", column_label="CreationDB", position=13, order=110, help="Creation DB Master/Replica")
        t.index('DumpLogId', [['DumpLogId', 'DESC']], area="Dyn_Index_1", primary=True, unique=True)
        t.index('CreateStart', [['CreateStart', 'DESC']], area="Dyn_Index_1")
        t.index('DumpId', [['DumpId', 'DESC'], ['CreateStart', 'DESC']], area="Dyn_Index_1")
        t.index('FileName', [['FileName', 'DESC']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('DumpLog')
