from gearbox.migrations import Migration

class AddTableServiceLCounter(Migration):

    database = "common"

    def up(self):
        t = self.table('ServiceLCounter', area="Sta_Data_128", table_trigger=[{'crc': '?', 'procedure': 'rd-servicelcounter.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-servicelcounter.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="servcou1")
        t.column('invseq', 'integer', format=">>>>>>>>>>9", initial="0", max_width=4, position=2, order=10)
        t.column('Amt', 'decimal', format=">>>>>>>>>>>>9.999", decimals=3, initial="0", max_width=18, position=5, order=40)
        t.column('MsSeq', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Mobsub", column_label="Msub", position=6, order=5, help="Link to mobsub-table")
        t.column('SLSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="SLSeq", column_label="SLseq", position=7, order=60, help="Sequence for Servicelimit")
        t.column('limit', 'integer', format=">>9", initial="0", help="limit", max_width=4, label="limit", column_label="limit", position=8, order=70, description="limit")
        t.column('Period', 'integer', format="999999", initial="0", max_width=4, label="Period", position=9, order=8, help="Period what Servicecounter is to be")
        t.column('BilledAmt', 'decimal', format="->>>>>>>>9.999", decimals=3, initial="0", max_width=18, label="Billed Amount", column_label="Billed", position=10, order=80, help="Billed amount")
        t.column('CustNum', 'integer', format=">>>>>>>9", initial=self.unknown, max_width=4, label="Customer", column_label="Cust", position=11, order=90, description="CustNum")
        t.column('Latest', 'decimal', format="99999999.999999", decimals=5, initial="0", max_width=20, label="Latest", column_label="Latest", position=12, order=100, help="Latest packet EDR")
        t.column('MSID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="MSID", column_label="MSID", position=13, order=110, description="MServicelimit ID")
        t.index('MSSeq', [['MsSeq'], ['Period', 'DESC'], ['SLSeq']], area="Sta_Index_3", primary=True)
        t.index('CustNum', [['CustNum'], ['Period', 'DESC'], ['SLSeq']], area="Sta_Index_2")

    def down(self):
        self.drop_table('ServiceLCounter')
