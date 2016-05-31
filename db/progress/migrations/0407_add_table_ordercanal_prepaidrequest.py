from gearbox.migrations import Migration

class AddTablePrePaidRequest(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('PrePaidRequest', area="Sta_Data_32", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-prepaidrequest.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-prepaidrequest.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="prepaidr")
        t.column('PPRequest', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="PPRequest", column_label="PPRequest", position=2, order=10)
        t.column('CLI', 'character', format="x(8)", initial="", max_width=16, label="CLI", column_label="CLI", position=3, order=20)
        t.column('Request', 'character', format="x(8)", initial="", max_width=16, label="Request", column_label="Request", position=4, order=31)
        t.column('CommLine', 'character', format="x(8)", initial="", max_width=16, position=5, order=40)
        t.column('Response', 'character', format="x(8)", initial="", max_width=16, label="Response", column_label="Response", position=6, order=50)
        t.column('TSRequest', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="TSRequest", column_label="TSRequest", position=7, order=60)
        t.column('TSResponse', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="TSResponse", column_label="TSResponse", position=8, order=70)
        t.column('PPStatus', 'integer', format=">>9", initial="0", max_width=4, label="Status", column_label="Status", position=9, order=80)
        t.column('TopUpAmt', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="TopUpAmount", column_label="TopUpAmount", position=10, order=90)
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=11, order=100, help="Code Of Brand")
        t.column('Source', 'character', format="x(8)", initial="", help="Source of request", max_width=16, label="Source", column_label="Source", position=12, order=110, description="Source of request")
        t.column('RespCode', 'integer', format=">>9", initial="0", max_width=4, label="ResponseCode", column_label="ResponseCode", position=13, order=120)
        t.column('Reference', 'character', format="x(8)", initial="", max_width=16, label="Reference", column_label="Reference", position=14, order=30)
        t.column('VatAmt', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="VatAmount", column_label="VatAmount", position=15, order=130)
        t.column('Fecha', 'character', format="x(8)", initial="", max_width=16, label="Fecha", column_label="Fecha", position=16, order=140, description="For ATM double checking")
        t.column('Entidad', 'character', format="x(8)", initial="", max_width=16, label="Entidad", column_label="Entidad", position=17, order=150, description="For ATM double checking")
        t.column('ClaveLocal', 'character', format="x(8)", initial="", max_width=16, label="ClaveLocal", column_label="ClaveLocal", position=18, order=160, description="For ATM double checking")
        t.column('OrigRequest', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Original Request ID", column_label="Orig.Request", position=19, order=170, help="Original request ID")
        t.column('TaxZone', 'character', format="x(8)", initial="", max_width=16, label="Tax Zone", column_label="TaxZone", position=20, order=180, help="Tax zone")
        t.column('UserCode', 'character', format="x(8)", initial="", max_width=16, label="User", column_label="User", position=21, order=190)
        t.column('PPReqPrefix', 'character', format="x(8)", initial="", max_width=16, label="Request Prefix", column_label="Prefix", position=22, order=200, help="Request prefix")
        t.column('MSSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Subscription ID", column_label="Subs.ID", position=23, order=210, help="Subscription ID")
        t.column('ReqCParam1', 'character', format="x(30)", initial="", max_width=60, label="Char Parameter 1", column_label="Char Parameter 1", position=24, order=220)
        t.index('PPRequest', [['Brand'], ['PPRequest']], area="Sta_Index_1", primary=True)
        t.index('ATM', [['Entidad'], ['Fecha'], ['ClaveLocal']], area="Sta_Index_1")
        t.index('CLI', [['Brand'], ['CLI'], ['TSRequest', 'DESC']], area="Sta_Index_1")
        t.index('MsSeq', [['Brand'], ['MSSeq'], ['TSRequest', 'DESC']], area="Sta_Index_1")
        t.index('OrigRequest', [['Brand'], ['OrigRequest']], area="Sta_Index_1")
        t.index('PPStatus', [['Brand'], ['PPStatus'], ['TSRequest', 'DESC']], area="Sta_Index_1")
        t.index('Reference', [['Brand'], ['Reference'], ['Request']], area="Sta_Index_1")
        t.index('Source', [['Brand'], ['Source'], ['TSRequest', 'DESC']], area="Sta_Index_1")

    def down(self):
        self.drop_table('PrePaidRequest')
