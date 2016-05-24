from gearbox.migrations import Migration

class AddTableMServiceLPool(Migration):

    database = "common"

    def up(self):
        t = self.table('MServiceLPool', area="Sta_Data_128", table_trigger=[{'crc': '?', 'procedure': 'rd-mservicelpool.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-mservicelpool.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="mservicelpool")
        t.column('SLSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="SLSeq", column_label="SLseq", position=2, order=10, help="Sequence for Servicelimit")
        t.column('MsSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Mobsub Sequence", column_label="MsSeq", position=3, order=20, help="Link to mobsub-table")
        t.column('LimitAmt', 'decimal', format=">>>>>>>>>>>>9.999", decimals=3, initial="0", max_width=18, label="Limit Amount", column_label="LimitAmt", position=4, order=30)
        t.column('FromTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="ValidFromTS", column_label="ValidFromTS", position=5, order=40, help="Valid From TimeStamp")
        t.column('EndTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="EndTS", column_label="EndTS", position=6, order=50, help="End TimeStamp")
        t.column('custnum', 'integer', format="->>>>>>>>9", initial="0", max_width=4, label="CustNum", column_label="CustNum", position=7, order=60, description="CustNum")
        t.index('MSSeq', [['MsSeq'], ['SLSeq'], ['EndTS', 'DESC']], area="Sta_Index_1", primary=True, unique=True)
        t.index('CustNum', [['custnum'], ['SLSeq'], ['EndTS', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MServiceLPool')
