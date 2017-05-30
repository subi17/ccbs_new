from gearbox.migrations import Migration

class AddTableDumpFile(Migration):

    database = "common"

    def up(self):
        t = self.table('DumpFile', area="Sta_Data_128", label="Dump File", dump_name="dumpfile", desc="Dump file")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('Description', 'character', format="x(50)", initial="", max_width=100, label="Description", position=3, order=20, help="Description of dump")
        t.column('DumpID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Dump ID", column_label="ID", position=4, order=30, help="Unique ID")
        t.column('FileName', 'character', format="x(50)", initial="", max_width=100, label="File Name", column_label="File", position=5, order=40, help="File name")
        t.column('SpoolDir', 'character', format="x(50)", initial="", max_width=100, label="Spool Directory", column_label="Spool", position=6, order=50, help="Spool directory")
        t.column('TransDir', 'character', format="x(50)", initial="", max_width=100, label="Transfer Directory", column_label="Transfer", position=7, order=60, help="Transfer directory")
        t.column('Active', 'logical', format="Yes/No", initial="yes", max_width=1, label="Active", position=8, order=70, help="Is dump active")
        t.column('DumpDelimiter', 'character', format="x(8)", initial="", max_width=16, label="Delimiter", position=9, order=80, help="Delimiter")
        t.column('DecimalPoint', 'character', format="x(8)", initial="", max_width=16, label="Decimal Point", column_label="Dec.Point", position=10, order=90, help="Decimal point (numeric-format)")
        t.column('MainTable', 'character', format="x(30)", initial="", max_width=60, label="Main Table", column_label="Table", position=11, order=100, help="Main table")
        t.column('SideTables', 'character', format="x(50)", initial="", max_width=100, label="Additional Tables", column_label="Side Tables", position=12, order=110, help="Additional tables")
        t.column('LinkKey', 'character', format="x(20)", initial="", max_width=40, label="Link Key", column_label="Key", position=13, order=120, help="Key field that is used to link the side tables to main table")
        t.column('DumpLineFeed', 'character', format="x(8)", initial="", max_width=16, label="Line Feed", position=14, order=130, help="Line feed between rows")
        t.column('DumpCharSet', 'character', format="x(20)", initial="", max_width=40, label="Character Set", column_label="Char.Set", position=15, order=140, help="Character set for the file")
        t.column('DumpName', 'character', format="x(20)", initial="", max_width=40, label="Dump Name", column_label="Name", position=16, order=150, help="Unique name for the dump")
        t.column('DumpFormat', 'character', format="x(8)", initial="", max_width=16, label="Dump Format", column_label="Format", position=17, order=160, help="Dump file format")
        t.column('EmptyFile', 'logical', format="Yes/No", initial="no", max_width=1, label="Create Empty File", column_label="Empty File", position=18, order=170, help="Create always a file even if events don't exist")
        t.column('FileCategory', 'character', format="x(12)", initial="", max_width=24, label="File Category", column_label="Category", position=19, order=180, help="File category")
        t.column('LogFile', 'character', format="x(40)", initial="", max_width=80, label="Log File", column_label="Log", position=20, order=190, help="Log file")
        t.column('QueryClause', 'character', format="x(30)", initial="", max_width=60, label="Query Clause", column_label="Query", position=21, order=200, help="Query clause used to retrieve records for the dump")
        t.column('UseIndex', 'character', format="x(20)", initial="", max_width=40, label="Use Index", column_label="Index", position=23, order=210, help="Index used for retrieving records")
        t.column('LogicModule', 'character', format="x(30)", initial="", max_width=60, label="Logic Module", column_label="Module", position=24, order=220, help="Module that contains logic for data collection")
        t.column('ModFromEventLog', 'logical', format="Yes/No", initial="no", max_width=1, label="Check EventLog", column_label="EventLog", position=25, order=230, help="Check modifications from EventLog")
        t.column('ModFromField', 'character', format="x(30)", initial="", max_width=60, label="Check Field", column_label="Field", position=26, order=240, help="Check modification from (timestamp) field")
        t.column('ModCollModule', 'character', format="x(30)", initial="", max_width=60, label="Collect Modified", column_label="Collection", position=27, order=250, help="Module for collecting modified events")
        t.column('FullCollModule', 'character', format="x(30)", initial="", max_width=60, label="Collect All", column_label="Full Coll.", position=28, order=260, help="Module for collecting all events for a full dump")
        t.column('EventLogFields', 'character', format="x(30)", initial="", max_width=60, label="EventLog Fields", column_label="Mod.Fields", position=29, order=280, help="Modified fields that will be checked from EventLog")
        t.column('ConfigParam', 'character', format="x(20)", initial="", max_width=40, label="Configuration Parameters", column_label="Parameter", position=30, order=270, help="Configuration parameters for the external procedures")
        t.column('AveDurMod', 'integer', format=">>>>>9", initial="0", max_width=4, label="AveDurMod", column_label="ModDur", position=31, order=300, help="Average Duration Modified")
        t.column('AllowReplica', 'logical', format="Yes/No", initial="No", max_width=1, label="AllowReplica", column_label="AllowReplica", position=32, order=310, help="Restriction Boolean value")
        t.column('AveDurFull', 'integer', format=">>>>>9", initial="0", max_width=4, label="AveDurFull", column_label="FullDur", position=33, order=290, help="Average Duration Full")
        t.column('BatchID', 'integer', format=">>9", initial="1", max_width=4, label="Batch ID", column_label="BatchID", position=34, order=320, help="The batch ID number of the dumpfile")
        t.index('DumpID', [['DumpID']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Description', [['Brand'], ['Description']], area="Sta_Index_2")
        t.index('DumpName', [['Brand'], ['DumpName']], area="Sta_Index_2")
        t.index('FileName', [['Brand'], ['FileName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('DumpFile')
