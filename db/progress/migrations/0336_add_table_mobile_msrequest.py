from gearbox.migrations import Migration

class AddTableMSRequest(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MSRequest', area="Sta_Data_64", label="MobSub Requests", table_trigger=[{'crc': '?', 'procedure': 'triggers/rd-msrequest.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'triggers/rw-msrequest.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="msreques", desc='''MobSub related action requests
''')
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="MobSub Sequence", column_label="SubSeq", position=3, order=20, help="Sequence for a subscription")
        t.column('Salesman', 'character', format="x(8)", initial="", max_width=16, label="Salesman", column_label="Sman", position=5, order=60, help="Salesman")
        t.column('ActStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Activation Time", column_label="Activate", position=6, order=70, help="Time when request will be processed")
        t.column('UserCode', 'character', format="x(8)", initial="", max_width=16, label="UserId", column_label="UserId", position=7, order=80, help="User who made the request")
        t.column('CreStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Creation Time", column_label="Created", position=8, order=90, help="Time when request was made")
        t.column('ReqStatus', 'integer', format="9", initial="0", max_width=4, label="Status", column_label="Stat", position=9, order=100, help="Status of request")
        t.column('ReqType', 'integer', format=">9", initial="0", max_width=4, label="Request Type", column_label="Type", position=10, order=110, help="Type of request")
        t.column('ReqCParam1', 'character', format="x(30)", initial="", max_width=60, label="Char Parameter 1", column_label="CParam1", position=11, order=120, help="Character parameter 1")
        t.column('ReqCParam2', 'character', format="x(30)", initial="", max_width=60, label="Char Parameter 2", column_label="CParam2", position=12, order=130, help="Character parameter 2")
        t.column('ReqDParam1', 'decimal', format="->>,>>9.99", decimals=5, initial="0", max_width=20, label="Dec Parameter 1", column_label="DParam1", position=13, order=140, help="Decimal parameter 1")
        t.column('ReqDParam2', 'decimal', format="->>,>>9.99", decimals=5, initial="0", max_width=20, label="Dec Parameter 2", column_label="DParam2", position=14, order=150, help="Decimal parameter 2")
        t.column('MsRequest', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Request ID", column_label="ID", position=15, order=160, help="Unique ID for request")
        t.column('CLI', 'character', format="X(15)", initial="", max_width=30, label="MSISDN", column_label="CLI", position=16, order=30, help="MSISDN subscriber number")
        t.column('DoneStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Handling Time", column_label="Handled", position=17, order=170, help="Time when request was handled (completed)")
        t.column('CreateFees', 'logical', format="Yes/No", initial="yes", max_width=1, label="Create Fees", column_label="Fees", position=18, order=190, help="Create fees that are associated to request")
        t.column('CustNum', 'integer', format=">>>>>>>9", initial="0", help="Customer number", max_width=4, label="Customer", column_label="Cust", position=19, order=40, description="mobsub.custnum")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=120, label="Memo", position=20, order=200, help="Memo")
        t.column('SendSMS', 'integer', format="9", initial="0", help="Send SMS when request is handled", max_width=4, label="Send SMS", column_label="SMS", position=21, order=180, description="0=no, 1=to user, 2=to owner, 3=both")
        t.column('Forced', 'logical', format="Yes/No", initial="no", max_width=1, label="Forced Run", column_label="Forced", position=22, order=210, help="Request has been run immediately, bypassing original scheduling")
        t.column('ReqIParam1', 'integer', format="->>>>>>>9", initial="0", max_width=4, label="Integer Parameter 1", column_label="IntParam1", position=23, order=220, help="Integer parameter 1")
        t.column('ReqIParam2', 'integer', format="->>>>>>>9", initial="0", max_width=4, label="Integer Parameter 2", column_label="IntParam2", position=24, order=230, help="Integer parameter 2")
        t.column('ReqCParam3', 'character', format="x(30)", initial="", max_width=60, label="Char Parameter 3", column_label="CParam3", position=25, order=240, help="Character parameter 3")
        t.column('ReqCParam4', 'character', format="x(30)", initial="", max_width=60, label="Char Parameter 4", column_label="CParam4", position=26, order=250, help="Character parameter 4")
        t.column('SoLog', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Solog ID", column_label="Solog", position=27, order=260, help="Sequence for Solog")
        t.column('OrigRequest', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Original Request ID", column_label="Orig.ID", position=28, order=270, help="ID of the original (father) request")
        t.column('ReqSource', 'character', format="x(8)", initial="", max_width=16, label="Source", position=29, order=280, help="Request source (orderer)")
        t.column('ReqIParam4', 'integer', format="->>>>>>>9", initial="0", max_width=4, label="Integer Parameter 4", column_label="IntParam4", position=30, order=300, help="Integer parameter 4")
        t.column('ReqDtParam1', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date parameter 1", column_label="DateParam1", position=31, order=310, help="Date parameter 1")
        t.column('ReqDtParam2', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date parameter 2", column_label="DateParam2", position=32, order=320, help="Date parameter 2")
        t.column('ReqIParam3', 'integer', format="->>>>>>>9", initial="0", max_width=4, label="Integer Parameter 3", column_label="IntParam3", position=33, order=290, help="Integer parameter 3")
        t.column('Mandatory', 'integer', format="9", initial="0", help="This request must be handled before father request can continue", max_width=4, label="Mandatory", column_label="Mand.", position=34, order=330, description='''

''')
        t.column('UpdateStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="UpdateStamp", column_label="Last updated", position=35, order=340, help="When this request was last updated")
        t.column('SMSText', 'character', format="x(12)", initial="", max_width=24, label="SMS Text", column_label="SMS", position=36, order=350, help="SMS text that is sent")
        t.column('ReqCParam5', 'character', format="x(30)", initial="", max_width=60, label="Char Parameter 5", column_label="CParam5", position=37, order=360, help="Character parameter 5")
        t.column('ReqIParam5', 'integer', format="->>>>>>>9", initial="0", max_width=4, label="Integer Parameter 5", column_label="IntParam5", position=38, order=370, help="Integer parameter 5")
        t.column('ReqCParam6', 'character', format="x(30)", initial="", max_width=60, label="Char Parameter 6", column_label="CParam6", position=39, order=380, help="Character parameter 6")
        t.index('MsRequest', [['MsRequest']], area="Sta_Index_3", primary=True, unique=True)
        t.index('BrandReq', [['Brand'], ['MsRequest', 'DESC']], area="Sta_Index_3")
        t.index('BrandType', [['Brand'], ['ReqType'], ['MsRequest', 'DESC']], area="Sta_Index_3")
        t.index('CLI', [['Brand'], ['ReqType'], ['CLI'], ['ActStamp', 'DESC']], area="Sta_Index_3")
        t.index('CustNum', [['Brand'], ['ReqType'], ['CustNum'], ['ActStamp', 'DESC']], area="Sta_Index_3")
        t.index('MsActStamp', [['MsSeq'], ['ActStamp', 'DESC']], area="Sta_Index_3")
        t.index('MsSeq', [['MsSeq'], ['ReqType'], ['ReqStatus']], area="Sta_Index_3")
        t.index('OrigRequest', [['OrigRequest']], area="Sta_Index_3")
        t.index('ReqCParam1', [['MsSeq'], ['ReqType'], ['ReqCParam1'], ['ReqStatus']], area="Sta_Index_3")
        t.index('ReqStatus', [['Brand'], ['ReqStatus'], ['ActStamp', 'DESC']], area="Sta_Index_3")
        t.index('ReqType', [['Brand'], ['ReqType'], ['ReqStatus'], ['ActStamp', 'DESC']], area="Sta_Index_3")
        t.index('UpdateStamp', [['Brand'], ['ReqStatus'], ['UpdateStamp', 'DESC']], area="Sta_Index_3")
        t.index('UserCode', [['Brand'], ['UserCode'], ['ActStamp', 'DESC']], area="Sta_Index_3")

    def down(self):
        self.drop_table('MSRequest')
