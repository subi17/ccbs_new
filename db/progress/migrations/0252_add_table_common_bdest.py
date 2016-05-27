from gearbox.migrations import Migration

class AddTableBDest(Migration):

    database = "common"

    def up(self):
        t = self.table('BDest', area="Sta_Data_128", label="B-Destinations", dump_name="bdest", desc="B-Destinations")
        t.column('BDest', 'character', mandatory=True, format="x(25)", initial="", max_width=50, label="B-subNo", column_label="B-subNo", position=2, order=10, help="B-number    ")
        t.column('BDName', 'character', format="x(30)", initial="", max_width=60, label="DestName", column_label="DestName", position=4, order=30, help="Destination's name ")
        t.column('CCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="Country", column_label="Country", position=5, order=40, help="Consecutive Country/Service Number")
        t.column('Class', 'integer', format="9", initial="1", max_width=4, label="Class", column_label="Cl ", position=6, order=50, help="Class: 1: A-sub pays 2: Freephone, B-sub pays")
        t.column('bt-prate', 'decimal', format="zz9.99999", decimals=5, initial="0", max_width=20, label="Rate/s", column_label="Rate/s", position=7, order=60, help="How much this service provider receives, 1/100 /Sec")
        t.column('bt-pros', 'integer', format="zz9", initial="0", max_width=4, label="Proc", column_label="Proc", position=8, order=70, help="How many % shall be paid to SP monthly")
        t.column('CDestArea', 'character', format="x(4)", initial="", max_width=8, label="Area Code", column_label="Acode", position=10, order=90, help="Area Code for c-number")
        t.column('CDest', 'character', format="x(15)", initial="", max_width=30, label="C-subNo", column_label="C-subNo", position=11, order=100, help="C-number")
        t.column('PNPCustNum', 'integer', mandatory=True, format=">>>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=12, order=110, help="Customer's number, 1 ... 999999")
        t.column('Free', 'logical', format="Yes/No", initial="no", max_width=1, label="Free", column_label="Free", position=13, order=120, help="Are calls to this B-number free of charge ?")
        t.column('DestType', 'integer', format=">9", initial="0", max_width=4, label="Type", column_label="Type", position=14, order=130, help="B-subscriber type, used with FTAM server.")
        t.column('FTAM', 'logical', format="Yes/No", initial="no", max_width=1, label="FTAM", column_label="FTAM", position=15, order=140, help="Send this b-number to FTAM configuration file")
        t.column('BillTarget', 'integer', format="z9", initial="0", max_width=4, label="Bill Target", column_label="BT", position=16, order=170, help="Customer's Billing Target")
        t.column('DiscGroup', 'character', format="x(8)", initial="", max_width=16, label="Discount Group", column_label="Discount Group", position=17, order=160, help="Discount Group Code")
        t.column('PNPBillTarget', 'integer', format="z9", initial="0", max_width=4, label="PNP Bill Target", column_label="PNP BT", position=18, order=180, help="PNP Customer's billing target")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=19, order=190, help="Code Of Brand")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=20, order=200, help="Last effective day")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=21, order=210, help="First effective day")
        t.column('BdestID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="BDestination ID", column_label="BDest ID", position=22, order=220, help="Unique ID")
        t.index('BDest', [['Brand'], ['BDest'], ['DestType'], ['ToDate', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('BDestID', [['BdestID']], area="Sta_Index_2", unique=True)
        t.index('BDName', [['Brand'], ['BDName'], ['BDest']], area="Sta_Index_2")
        t.index('CCN', [['Brand'], ['CCN'], ['BDName'], ['BDest']], area="Sta_Index_2")

    def down(self):
        self.drop_table('BDest')
