from gearbox.migrations import Migration

class AddTableMServiceLimit(Migration):

    database = "common"

    def up(self):
        t = self.table('MServiceLimit', area="Sta_Data_128", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-mservicelimit.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-mservicelimit.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="mservice")
        t.column('SLSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="SLSeq", column_label="SLseq", position=2, order=10, help="Sequence for Servicelimit")
        t.column('MsSeq', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Mobsub", column_label="Msub", position=3, order=20, help="Link to mobsub-table")
        t.column('DialType', 'integer', format=">>9", initial="0", max_width=4, label="Dialling Type", column_label="DT", position=4, order=30, help="Dialling type code")
        t.column('InclUnit', 'integer', format=">9", initial="0", max_width=4, label="Included Unit", column_label="Incl.Unit", position=5, order=40, help="Unit of included material")
        t.column('InclAmt', 'decimal', format="->>>>>9.99", decimals=2, initial="0", max_width=17, label="Included Amount", column_label="Incl.Amt", position=6, order=50, help="Amount of billable material that is included in this fee")
        t.column('FromTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="ValidFromTS", column_label="ValidFromTS", position=7, order=60, help="Valid From TimeStamp")
        t.column('EndTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="EndTS", column_label="EndTS", position=8, order=70, help="End TimeStamp")
        t.column('custnum', 'integer', format="->>>>>>>>9", initial="0", max_width=4, label="CustNum", column_label="CustNum", position=9, order=80, description="CustNum")
        t.column('MSID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="MSID", column_label="MSID", position=10, order=90, description="MServicelimit ID")
        t.index('msseq', [['MsSeq'], ['DialType'], ['SLSeq'], ['EndTS', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Active', [['MsSeq'], ['DialType'], ['FromTS'], ['EndTS']], area="Sta_Index_2")
        t.index('CustNum', [['custnum'], ['DialType'], ['SLSeq'], ['EndTS', 'DESC']], area="Sta_Index_2")
        t.index('slseq', [['SLSeq'], ['DialType'], ['EndTS', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MServiceLimit')
