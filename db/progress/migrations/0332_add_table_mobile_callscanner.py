from gearbox.migrations import Migration

class AddTableCallScanner(Migration):

    database = "mobile"

    def up(self):
        t = self.table('CallScanner', area="Sta_Data_32", dump_name="callscan")
        t.column('TMSTime', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Timestamp", max_width=20, label="TMS Time", column_label="TMS Time", position=2, order=10, description='''

''')
        t.column('UserCode', 'character', format="x(8)", initial="", help="User ID who makes the CallScanner event", max_width=16, label="UserId", column_label="UserId", position=3, order=20, description='''


''')
        t.column('SystemID', 'character', format="x(8)", initial="", help="System ID", max_width=16, label="SystemID", column_label="SystemID", position=4, order=30, description='''
''')
        t.column('EventType', 'character', format="x(8)", initial="", help="EventType", max_width=16, label="EventType", column_label="EventType", position=5, order=40, description='''

''')
        t.column('ReasonCode', 'character', format="x(8)", initial="", help="Reason code", max_width=16, label="ReasonC", column_label="ReasonC", position=6, order=50, description='''

''')
        t.column('Level', 'character', format="x(8)", initial="", help="Level", max_width=16, label="Level", column_label="Level", position=7, order=60, description='''

''')
        t.column('Target', 'character', format="x(20)", initial="", help="Target", max_width=40, label="Target", column_label="Target", position=8, order=70, description='''
''')
        t.column('StartTime', 'character', format="x(20)", initial="", help="StartTime From", max_width=40, label="StartTime", column_label="StartTime", position=9, order=80, description='''

''')
        t.column('EndTime', 'character', format="x(20)", initial="", help="EndTime to", max_width=40, label="EndTime", column_label="EndTime", position=10, order=90, description='''

''')
        t.column('SearchRule', 'character', format="x(40)", initial="", help="SearchRule", max_width=80, label="SearchRule", column_label="SearchRule", position=11, order=100, description='''

''')
        t.column('AccessType', 'character', format="x(10)", initial="", help="Access Type R/W", max_width=20, label="Access Type", column_label="AccessType", position=12, order=110, description="Type of access")
        t.index('TMSTime', [['TMSTime', 'DESC']], area="Sta_Index_1", primary=True)
        t.index('ReasonCode', [['ReasonCode'], ['TMSTime', 'DESC']], area="Sta_Index_1")
        t.index('UserCode', [['UserCode']], area="Sta_Index_1")

    def down(self):
        self.drop_table('CallScanner')
