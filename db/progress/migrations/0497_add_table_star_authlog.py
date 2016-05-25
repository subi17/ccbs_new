from gearbox.migrations import Migration

class AddTableAuthLog(Migration):

    database = "star"

    def up(self):
        t = self.table('AuthLog', area="Sta_Data_128", label="AuthLog", dump_name="authlog", desc="3rd party user authentication log")
        t.column('Username', 'character', format="x(20)", initial="", max_width=40, label="Username", column_label="Username", position=2, order=10)
        t.column('IP', 'character', format="x(16)", initial="", max_width=32, label="IP Address", column_label="IP Address", position=3, order=20)
        t.column('EndUserID', 'character', format="x(40)", initial="", max_width=80, label="EndUserID", column_label="EndUserID", position=4, order=30)
        t.column('UserAgent', 'character', format="x(40)", initial="", max_width=80, label="UserAgent", column_label="UserAgent", position=5, order=40)
        t.column('MethodName', 'character', format="x(40)", initial="", max_width=80, label="MethodName", column_label="MethodName", position=6, order=50)
        t.column('ErrorMsg', 'character', format="x(60)", initial="", max_width=120, label="ErrorMsg", column_label="ErrorMsg", position=7, order=60)
        t.column('TimeStamp', 'datetime', format="99-99-9999 HH:MM:SS", max_width=8, label="TimeStamp", column_label="TimeStamp", position=8, order=70)
        t.column('ResponseTS', 'datetime', format="99-99-9999 HH:MM:SS", max_width=8, label="ResponseTS", column_label="ResponseTS", position=9, order=80)
        t.column('ErrorCode', 'integer', format="->>>>9", initial="0", max_width=4, label="ErrorCode", column_label="ErrorCode", position=10, order=90, help="XML-RPC framework error code")
        t.column('TransactionID', 'character', format="x(15)", initial="", max_width=30, label="TransactionID", position=11, order=100, help="Transaction ID")
        t.index('TimeStamp', [['TimeStamp', 'DESC']], area="Sta_Index_3", primary=True)
        t.index('Username', [['Username']], area="Sta_Index_3")

    def down(self):
        self.drop_table('AuthLog')
