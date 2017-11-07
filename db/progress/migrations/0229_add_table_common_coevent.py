from gearbox.migrations import Migration

class AddTableCoEvent(Migration):

    database = "common"

    def up(self):
        t = self.table('CoEvent', area="Sta_Data_64", label="Commission Event", dump_name="coevent", desc="Commission Event")
        t.column('HostTable', 'character', mandatory=True, format="x(12)", initial="", max_width=24, label="HostTable", column_label="Table", position=2, order=10, help="Name of table for which event is calculated")
        t.column('HostKey', 'character', format="x(10)", initial="", max_width=20, label="Host Key", column_label="Key", position=3, order=20, help="Key of table for which event is calculated")
        t.column('CoRuleID', 'integer', format=">>>>>9", initial="0", max_width=4, label="Rule ID", column_label="RuleID", position=4, order=30, help="Commission rule ID")
        t.column('CalcDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Calc. Date", column_label="Date", position=5, order=40, help="Date when event was created")
        t.column('Salesman', 'character', format="x(8)", initial="", max_width=16, label="Salesman", column_label="Sman", position=6, order=50, help="Salesman for which commission is paid")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Cust", position=7, order=60, help="Customer for which commission is paid")
        t.column('CommAmt', 'decimal', format="->>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Commission", column_label="Amt", position=8, order=70, help="Commission amount")
        t.column('PaymDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Payment Date", column_label="Paid", position=9, order=80, help="Date when commission has been paid out")
        t.column('CoEventID', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="EventID", column_label="ID", position=10, order=90, help="Unique event ID")
        t.column('CoTargId', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Target ID", column_label="TargID", position=11, order=100, help="Commission target ID")
        t.column('CommFrom', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="From Date", column_label="From", position=12, order=110, help="Commission is based on events from this date onwards")
        t.column('CommTo', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To Date", column_label="To", position=13, order=120, help="Commission is based on events till this date")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=14, order=131, help="Code Of Brand")
        t.column('CoAmt', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="Fixed Amount", column_label="Fixed", position=15, order=140, help="Fixed commission amount")
        t.column('CoPerc', 'decimal', format=">9.99", decimals=2, initial="0", max_width=17, label="Commission %", column_label="Comm%", position=16, order=150, help="Commission percentage")
        t.column('BaseAmt', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Base Amount", column_label="Base", position=17, order=130, help="Base amount for commission (% is calculated from this)")
        t.index('HostTable', [['Brand'], ['HostTable'], ['HostKey']], area="Sta_Index_2", primary=True)
        t.index('CalcDate', [['Brand'], ['CalcDate'], ['CoEventID']], area="Sta_Index_2")
        t.index('CoEventID', [['CoEventID']], area="Sta_Index_2", unique=True)
        t.index('CoRuleID', [['Brand'], ['CoRuleID']], area="Sta_Index_2")
        t.index('CoTargID', [['CoTargId']], area="Sta_Index_2")
        t.index('CustNum', [['Brand'], ['CustNum']], area="Sta_Index_2")
        t.index('CustNum_s', [['CustNum']], area="Sta_Index_2")
        t.index('PaymDate', [['Brand'], ['PaymDate', 'DESC'], ['CoEventID']], area="Sta_Index_2")
        t.index('Salesman', [['Brand'], ['Salesman'], ['PaymDate', 'DESC'], ['CoEventID']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CoEvent')
