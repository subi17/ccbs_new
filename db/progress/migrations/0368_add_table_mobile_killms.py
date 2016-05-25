from gearbox.migrations import Migration

class AddTableKillMs(Migration):

    database = "mobile"

    def up(self):
        t = self.table('KillMs', area="Sta_Data_64", label="Request for KILL of a mobsub", dump_name="killms", desc="requests for scheduled deactivations of mob subscribers")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="SubSeq", column_label="SubSeq", position=2, order=10, help="Sequence for a Subscription")
        t.column('RequestTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="ReqTimeSt", column_label="ReqTimeSt", position=3, order=20, help="Time stamp when request was saved")
        t.column('ExecuteTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="ExecTs", column_label="ExecTs", position=4, order=30, help="Time Stamp when request was executed")
        t.column('UserCode', 'character', format="x(8)", initial="", max_width=16, label="UserId", column_label="UserId", position=5, order=40, help="User ID who made the KILL request")
        t.column('KillDate', 'date', format="99.99.99", max_width=4, label="KillDate", column_label="KillDate", position=6, order=50, help="Date when subscription shall be killed")
        t.column('Stat', 'integer', format="9", initial="0", max_width=4, label="Status", column_label="St", position=7, order=60, help="Status 1: Pending 2: Failed 3: Done")
        t.column('CLI', 'character', format="X(11)", initial="", max_width=22, label="MSISDN", column_label="MSISDN No", position=8, order=70, help="MSISDN Subscriber No")
        t.column('ErrorMsg', 'character', format="x(78)", initial="", max_width=156, label="Error msg", column_label="Error msg", position=9, order=80, help="Explanation why execution failed")
        t.column('KeepSIM', 'logical', format="Yes/No", initial="no", max_width=1, label="Keep SIM", column_label="Keep SIM", position=10, order=90, help="Retain SIM card of de-activated mobile subscription (Y/N) ?")
        t.column('Stock', 'character', format="x(8)", initial="", max_width=16, label="Stock", column_label="Stock", position=11, order=100, help="Return SIM into stock (enter stock code)")
        t.column('OutOper', 'character', format="x(12)", initial="", max_width=24, label="Operator", column_label="Operator", position=13, order=120, help="Operator to whom this MSDN shall be outported")
        t.column('KillTime', 'decimal', format="99.99", decimals=2, initial="0", max_width=17, label="KTime", column_label="KTime", position=14, order=130, help="Time when subscription should be killed")
        t.column('KillDateTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="KillTS", column_label="KillTS", position=15, order=140, help="Kill Date&Time in a Time Stamp Format")
        t.column('SoSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="OrdSeq", column_label="OrdSeq", position=16, order=150, help="Sequence for the corresponding ""DELETE"" Service Order")
        t.column('OutPort', 'logical', format="yes/no", initial="no", max_width=1, position=17, order=160, help="Shall this MSISDN be OUTported (MNP) when killed Y/N ?")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=18, order=170, help="Code Of Brand")
        t.index('CLI', [['Brand'], ['CLI'], ['MsSeq']], area="Sta_Index_3", primary=True, unique=True)
        t.index('CLI_s', [['CLI'], ['MsSeq']], area="Sta_Index_3", unique=True)
        t.index('killdate', [['Brand'], ['KillDate', 'DESC'], ['CLI']], area="Sta_Index_3", unique=True)
        t.index('MsSeq', [['MsSeq'], ['Stat']], area="Sta_Index_3", unique=True)
        t.index('Stat', [['Brand'], ['Stat'], ['KillDate'], ['CLI']], area="Sta_Index_3")

    def down(self):
        self.drop_table('KillMs')
