from gearbox.migrations import Migration

class AddTableMobSub(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('MobSub', area="Sta_Data_32", label="Mobile Subscriber", table_trigger=[{'crc': '?', 'procedure': 'rd-mobsub.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-mobsub.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="mobsub-1", desc="Mobile Subscription")
        t.column('MsSeq', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="SubSeq", column_label="SubSeq", position=2, order=10, help="Sequence for a Subscription")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=3, order=20, help="Customer's number")
        t.column('CLI', 'character', format="X(11)", initial="", max_width=22, label="MSISDN", column_label="MSISDN No", position=5, order=40, help="MSISDN Subscriber No")
        t.column('IMSI', 'character', format="x(18)", initial="", max_width=36, label="IMSI Number", column_label="IMSI Number", position=7, order=60, help="IMSI Number")
        t.column('ICC', 'character', format="x(20)", initial="", max_width=40, label="SIM Serial", column_label="SIM Serial no. (ICC)", position=8, order=70, help="Serial no. (ICC) of an individual SIM card")
        t.column('CreationDate', 'date', format="99-99-99", max_width=4, label="Created", position=9, order=80, help="Date when subscription was created (from order)")
        t.column('Salesman', 'character', format="x(8)", initial="", max_width=16, label="Salesman", column_label="Salesman", position=11, order=100, help="Salesman's code")
        t.column('Reseller', 'character', format="x(8)", initial="", max_width=16, label="Reseller", column_label="Reseller", position=12, order=110, help="An unique code for a reseller")
        t.column('Contract', 'logical', format="Yes/No", initial="no", max_width=1, label="ContSign", column_label="ContSign", position=13, order=120, help="Contract signed")
        t.column('ActivationDate', 'date', format="99-99-99", max_width=4, label="Activation Date", column_label="Activation", position=15, order=140, help="Date when subscription is to be activated in HLR")
        t.column('MsStatus', 'integer', format="9", initial="1", max_width=4, label="Status", column_label="Status", position=16, order=150, help="Status Code, 1 ... 9")
        t.column('RepCodes', 'character', format="x(30)", initial="", max_width=60, label="ReportCode", column_label="ReportCode", position=25, order=260, help="Report Codes")
        t.column('IDCode', 'character', format="x(4)", initial="", help="Identification Code", max_width=8, label="ID Code", column_label="ID Code", position=26, order=810, description="4 digit code used to identify subscription")
        t.column('SegmentCode', 'character', format="x(8)", initial="", max_width=16, label="Segmentation Code", column_label="SegmentCode", position=27, order=820, help="Segmentation code")
        t.column('SegmentDate', 'date', format="99-99-99", max_width=4, label="Segmentation Date", column_label="SegmentDate", position=28, order=831, help="Date when segmentation code was changed")
        t.column('SegmentOffer', 'character', format="x(8)", initial="", max_width=16, label="Segmentation Renewal Offer", column_label="SegmentOffer", position=29, order=830)
        t.column('MultiSimID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Multi Sim ID", column_label="Multi Sim ID", position=31, order=840, help="Multi Sim ID")
        t.column('MultiSimType', 'integer', format=">>>9", initial="0", max_width=4, label="Multi Sim Type", column_label="Multi Sim Type", position=32, order=850, help="Multi Sim type (1=primary, 2=secondary)")
        t.column('BarrCode', 'character', format="x(10)", initial="", max_width=20, label="BarrCode", column_label="BarrCode", position=33, order=860)
        t.column('TariffBundle', 'character', format="x(10)", initial="", max_width=20, label="Tariff Bundle", column_label="TariffBundle", position=34, order=870)
        t.column('TariffActDate', 'date', format="99-99-99", max_width=4, label="Tariff Activation Date", column_label="TariffActDate", position=35, order=880)
        t.column('TariffActTS', 'decimal', format="99999999.99999", initial="0", max_width=15, label="TariffActTS", column_label="TariffActTS", position=36, order=890)
        t.column('CliType', 'character', format="x(8)", initial="", max_width=16, label="MType", column_label="Mtype", position=43, order=440, help="Type Of Mobsub")
        t.column('ActivationTS', 'decimal', format="999999.99999", decimals=5, initial="0", max_width=20, label="Activated", position=48, order=480, help="Time when subscription was activated in TMS (HLR response OK)")
        t.column('SIMDelDate', 'date', format="99-99-99", max_width=4, label="DeliDate", column_label="DeliDate", position=49, order=490, help="Date When Sim Card delivery request was sent")
        t.column('SimDelStatus', 'integer', format="9", initial="0", max_width=4, label="DeliStat", column_label="DeliStat", position=50, order=500, help="Status of SIM Card Delivery")
        t.column('SIMActDate', 'date', format="99-99-99", max_width=4, label="sadate", column_label="sadate", position=51, order=510, help="Date When Sim delevery request was saved")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=52, order=520, help="Code Of Brand")
        t.column('BillTarget', 'integer', format=">9", initial="0", max_width=4, label="Bill.Target", column_label="BT", position=53, order=530, help="Customer's billing target")
        t.column('ServiceChanges', 'integer', format=">9", initial="0", max_width=4, label="ServiceChanges", column_label="ServiceChanges", position=54, order=550, help="Indicates if user is allowed to do changes (0,1,2,3)")
        t.column('MNPChannel', 'integer', format=">9", initial="0", help="MNP Channel", max_width=4, label="MNP Channel", column_label="MNPChannel", position=77, order=770, description='''

''')
        t.column('AgrCust', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Agr.Customer", column_label="AgrCust", position=78, order=790, help="Agreement customer's number")
        t.column('InvCust', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Inv.Customer", column_label="InvCust", position=79, order=780, help="Invoicing customer's number")
        t.column('PayType', 'logical', format="PrePaid/PostPaid", initial="no", help="PayType", max_width=1, label="PayType", column_label="PayType", position=80, order=800, description='''

''')
        t.column('SegmentCons', 'decimal', format=">>>9.999", decimals=5, initial="0", max_width=20, label="SegmentCons", position=81, order=832, help="Average consumption in euros")
        t.index('MsSeq', [['MsSeq']], area="Sta_Index_1", primary=True, unique=True)
        t.index('AgrCust', [['Brand'], ['AgrCust'], ['CLI']], area="Sta_Index_1")
        t.index('CLI', [['Brand'], ['CLI']], area="Sta_Index_1", unique=True)
        t.index('CLIType', [['Brand'], ['CliType'], ['CLI']], area="Sta_Index_1")
        t.index('CLI_u', [['CLI']], area="Sta_Index_1", unique=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['BillTarget'], ['CLI']], area="Sta_Index_1", unique=True)
        t.index('CustNum_u', [['CustNum'], ['BillTarget'], ['CLI']], area="Sta_Index_1", unique=True)
        t.index('IMSI', [['Brand'], ['IMSI']], area="Sta_Index_1")
        t.index('InvCust', [['Brand'], ['InvCust'], ['CLI']], area="Sta_Index_1")
        t.index('MNPChannel', [['Brand'], ['MNPChannel'], ['MsStatus']], area="Sta_Index_1")
        t.index('MsStatus', [['Brand'], ['MsStatus'], ['ActivationDate']], area="Sta_Index_1")
        t.index('MsStatus_u', [['MsStatus'], ['ActivationDate']], area="Sta_Index_1")
        t.index('MultiSimID', [['Brand'], ['MultiSimID', 'DESC']], area="Dyn_Index_1")

    def down(self):
        self.drop_table('MobSub')
