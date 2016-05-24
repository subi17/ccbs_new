from gearbox.migrations import Migration

class AddTableMSISDN(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('MSISDN', area="Sta_Data_256", dump_name="msisdn", desc="A Single MSISDN number")
        t.column('CLI', 'character', format="X(11)", initial="", max_width=22, label="MSISDN", column_label="MSISDN No", position=2, order=10, help="MSISDN Subscriber No")
        t.column('StatusCode', 'integer', format="z9", initial="1", max_width=4, label="Status", column_label="Status", position=3, order=20, help="Status Code")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=5, order=40, help="Customer's number IF this MSISDN is delivered to a customer")
        t.column('MNP', 'logical', format="MNP/", initial="FALSE", max_width=1, label="MNP", column_label="MNP", position=7, order=26, help="MNP Number")
        t.column('ValidFrom', 'decimal', format="99999999.99999", decimals=5, initial="99999999,99999", max_width=20, label="Usage valid", column_label="Usage valid", position=8, order=22, help="Time Stamp, when usage begin")
        t.column('McCode', 'integer', format="zz9", initial="1", max_width=4, label="Class", column_label="Class", position=9, order=80, help="Code of MSISDN Class")
        t.column('PrevMuSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="SeqNo", column_label="SeqNo", position=12, order=110, help="Internal, consecutive sequence no of PREVIOUS user")
        t.column('ActionDate', 'date', format="99-99-99", max_width=4, label="ActionDate", column_label="PortDate", position=13, order=28, help="Date when action happens")
        t.column('PortingTime', 'decimal', format="99.99", decimals=2, initial="0", max_width=17, label="Ptime", column_label="Ptime", position=14, order=130, help="Time when this MSISDN can be activated (INported) earliest")
        t.column('OutOperator', 'character', format="x(12)", initial="", max_width=24, label="OutOp", column_label="OutOp", position=15, order=140, help="Name of operator to whom this MSISDN was OUTported")
        t.column('OrderID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="OrderId", column_label="OrderId", position=17, order=160, description="Order sequence number")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=18, order=170, help="Code Of Brand")
        t.column('MsisdnType', 'integer', format=">9", initial="0", max_width=4, label="MSISDNType", column_label="MSISDNType", position=19, order=180, help="MSISDN Type")
        t.column('LockedTo', 'decimal', format="999999.99999", decimals=2, initial="0", max_width=17, position=20, order=190)
        t.column('MSSeq', 'integer', format="zzzzzzzz9", max_width=4, label="MSSeq", column_label="MSSEq", position=21, order=21, help="Sequence for subscriptions")
        t.column('PayType', 'logical', format="PrePaid/PostPaid", initial="FALSE", max_width=1, label="PayType", column_label="PayType", position=22, order=27, help="PayType")
        t.column('ValidTo', 'decimal', format="99999999.99999", decimals=5, initial="99999999,99999", max_width=20, label="Usage valid", column_label="Usage valid to", position=23, order=24, help="Time Stamp, when usage end")
        t.column('PortingDate', 'date', format="99-99-99", max_width=4, label="PortDate", column_label="PortDate", position=24, order=120, help="Date when this MSISN no. was ported in or out")
        t.column('POS', 'character', format="x(12)", initial="", max_width=24, label="POS", column_label="POS", position=25, order=29, help="Point of Sales")
        t.index('CLI', [['Brand'], ['CLI'], ['ValidFrom', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('ActionDate', [['Brand'], ['StatusCode'], ['ActionDate']], area="Sta_Index_1")
        t.index('CLI_s', [['CLI']], area="Sta_Index_2")
        t.index('CustNum', [['Brand'], ['CustNum'], ['ValidFrom', 'DESC']], area="Sta_Index_1")
        t.index('MSSeq', [['MSSeq'], ['ValidFrom', 'DESC']], area="Sta_Index_2")
        t.index('OrderID', [['Brand'], ['OrderID', 'DESC']], area="Sta_Index_2")
        t.index('POS', [['POS'], ['StatusCode'], ['CLI'], ['ValidFrom', 'DESC']], area="Sta_Index_1")
        t.index('POSDCLI', [['POS'], ['StatusCode'], ['CLI', 'DESC'], ['ValidTo', 'DESC']], area="Sta_Index_2")
        t.index('StatusCode', [['Brand'], ['StatusCode'], ['CLI'], ['ValidFrom', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MSISDN')
