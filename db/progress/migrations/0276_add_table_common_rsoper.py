from gearbox.migrations import Migration

class AddTablersoper(Migration):

    database = "common"

    def up(self):
        t = self.table('rsoper', area="Sta_Data_256", label="Reseller Operators", dump_name="rsoper", desc="Reseller operators")
        t.column('CustNum', 'integer', mandatory=True, format="zzzzzz9", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=2, order=10, help="Customer's number")
        t.column('Operator', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="OpCode", column_label="OpCode", position=3, order=20, help="Operator's code, 1 - 8 characters")
        t.column('fileid', 'character', format="x(4)", initial="", max_width=8, label="FileID", column_label="FileID", position=4, order=30, help="File identification prefix")
        t.column('req-script', 'character', format="x(20)", initial="", max_width=40, label="REQ Script", column_label="REQ Script", position=5, order=40, help="Name of the request script")
        t.column('cdr-script', 'character', format="x(20)", initial="", max_width=40, label="CDR Script", column_label="CDR Script", position=6, order=50, help="Name of the CDR script")
        t.column('cps-script', 'character', format="x(20)", initial="", max_width=40, label="CPS Script", column_label="CPS Script", position=7, order=60, help="Name of the CPS script")
        t.column('Reseller', 'integer', format="99", initial="0", max_width=4, label="Code", column_label="Code", position=8, order=70, help="Reseller code")
        t.column('cli-pref', 'character', format="x(4)", initial="", max_width=8, label="Prefix", column_label="Prefix", position=9, order=80, help="Partners prefix")
        t.column('rs-dir', 'character', format="x(20)", initial="", max_width=40, label="Home directory", column_label="Home directory", position=10, order=90, help="Home directory, ie. whereunder the subdirectories are")
        t.column('pstype', 'integer', valexp="INPUT pstype < 4", format="9", initial="0", max_width=4, label="Type", column_label="Type", position=11, order=100, valmsg="Type of preselction MUST be 0 ... 3 !", help="Type of presel. 0):None 1):Nat 2):Intn'l 3):Both")
        t.column('res-script', 'character', format="x(20)", initial="", max_width=40, label="RES Script", column_label="RES Script", position=12, order=110, help="Name of the response script")
        t.column('wl-fileid', 'character', format="x(4)", initial="", max_width=8, label="FileID", column_label="FileID", position=13, order=120, help="File identification prefix")
        t.column('Interval', 'integer', format="z9", initial="0", max_width=4, label="Interval", column_label="Int.", position=14, order=130, help="Interval for files in hours")
        t.column('CDRSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="CDR Seq", column_label="CDR Seq", position=15, order=140, help="CDR file sequence number")
        t.index('CustNum', [['CustNum'], ['Operator']], area="Sta_Index_2", primary=True, unique=True)
        t.index('interval', [['Interval'], ['fileid']], area="Sta_Index_2")
        t.index('Operator', [['Operator'], ['CustNum']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('rsoper')
