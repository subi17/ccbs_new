from gearbox.migrations import Migration

class AddTableStarSession(Migration):

    database = "star"

    def up(self):
        t = self.table('StarSession', area="Sta_Data_256", dump_name="starsess")
        t.column('SessionId', 'integer', format="->,>>>,>>>,>>9", initial="0", max_width=4, label="SessionId", column_label="Id", position=2, order=10, help="SessionId")
        t.column('SessionConnectionId', 'character', format="X(78)", initial="", max_width=40, label="Connection Id", position=3, order=30, help="Session Connection Id")
        t.column('machinetime', 'integer', format="->,>>>,>>>,>>9", initial="0", max_width=4, label="Machinetime", position=4, order=50, help="Machinetime since session begin.. ETIME function")
        t.column('LastDate', 'date', format="99.99.99", max_width=4, label="LastDate", position=5, order=80, help="LastDate")
        t.column('LastTime', 'character', format="X(8)", initial="", max_width=16, label="LastTime", position=6, order=90, help="Last time")
        t.column('usercode', 'character', format="X(12)", initial="", max_width=24, label="Usercode", position=7, order=40, help="Usercode")
        t.column('data', 'character', format="X(78)", initial="", max_width=156, label="Global data", position=8, order=100, help="Global data")
        t.column('clienttype', 'character', format="X(12)", initial="", max_width=24, label="Client Type", position=9, order=20, help="Client type")
        t.column('CreateDate', 'date', format="99.99.99", max_width=4, label="CreateDate", position=10, order=60)
        t.column('CreateTime', 'character', format="X(8)", initial="", max_width=16, label="CreateTime", position=11, order=70)
        t.index('SessionId', [['SessionId']], area="Sta_Index_2", primary=True, unique=True)
        t.index('LastChange', [['LastDate'], ['machinetime']], area="Sta_Index_2")
        t.index('SessionConnectionId', [['clienttype'], ['SessionConnectionId'], ['CreateDate'], ['CreateTime']], area="Sta_Index_2")
        t.index('usercode', [['usercode'], ['LastDate'], ['LastTime']], area="Sta_Index_2")

    def down(self):
        self.drop_table('StarSession')
