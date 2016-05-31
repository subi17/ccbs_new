from gearbox.migrations import Migration

class AddTableServiceLimit(Migration):

    database = "common"

    def up(self):
        t = self.table('ServiceLimit', area="Sta_Data_64", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-servicelimit.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-servicelimit.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="service1")
        t.column('WebDisp', 'integer', format="9", initial="1", max_width=4, label="Web", column_label="Web", position=9, order=110, help="Web")
        t.column('GroupCode', 'character', format="x(16)", initial="", max_width=32, label="GroupCode", column_label="Group Code", position=10, order=90, help="Group Code of Servicelimit")
        t.column('ToTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="ToTimestamp", column_label="ToTimestamp", position=11, order=80, help="to timestamp")
        t.column('DialType', 'integer', format=">>9", initial="0", max_width=4, label="Dialling Type", column_label="DT", position=12, order=111, help="Dialling type code")
        t.column('SLCode', 'character', format="x(12)", initial="", max_width=24, label="ServiceLimit", column_label="ServiceLimit", position=13, order=120, help="Code of Servicelimit")
        t.column('SLName', 'character', format="x(30)", initial="", max_width=60, label="Servicelimit Name", column_label="Servicelimit Name", position=14, order=130, help="Name of Service limit")
        t.column('InclUnit', 'integer', format=">9", initial="0", max_width=4, label="Included Unit", column_label="Incl.Unit", position=15, order=140, help="Unit of included material")
        t.column('InclAmt', 'decimal', format="->>>>>9.99", decimals=2, initial="0", max_width=17, label="Included Amount", column_label="Incl.Amt", position=16, order=150, help="Amount of billable material that is included in this fee")
        t.column('SLSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="SLSeq", column_label="SLseq", position=17, order=100, help="Sequence for Servicelimit")
        t.column('ValidFrom', 'date', format="99-99-99", initial=self.unknown, help="Valid from this date on", max_width=4, label="Valid From", position=18, order=160, description='''

''')
        t.column('ValidTo', 'date', format="99-99-99", initial=self.unknown, help="Valid until this date", max_width=4, label="Valid To", position=19, order=170, description='''
''')
        t.column('FirstMonthCalc', 'integer', format=">9", initial="0", max_width=4, label="First Month Calculation", column_label="1.Month", position=20, order=180, help="First month calculation method")
        t.column('LastMonthCalc', 'integer', format=">9", initial="0", max_width=4, label="Last Month Calculation", column_label="Last Month", position=21, order=190, help="Last month calculation method")
        t.column('BDestLimit', 'integer', format=">>>>9", initial="0", max_width=4, label="BDestLimit", position=22, order=200, help="BDestLimit")
        t.index('GroupCode', [['GroupCode']], area="Sta_Index_2", primary=True)
        t.index('SLSeq', [['SLSeq']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('ServiceLimit')
