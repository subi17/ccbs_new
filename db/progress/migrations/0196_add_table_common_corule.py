from gearbox.migrations import Migration

class AddTableCoRule(Migration):

    database = "common"

    def up(self):
        t = self.table('CoRule', area="Sta_Data_64", label="Commission Rule", dump_name="corule", desc="Defined rules for the commission to be paid to Salesman.")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer's number")
        t.column('BillCode', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillItem", position=4, order=30, help="Billing Item code")
        t.column('coFrom', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Commission From", column_label="From", position=5, order=40, help="Valid from")
        t.column('coTo', 'date', format="99-99-9999", initial=self.unknown, max_width=4, label="Commission To", column_label="To", position=6, order=50, help="Valid to")
        t.column('coNoInst', 'integer', format=">9", initial="0", max_width=4, label="No. of Instalments", column_label="No. of Inst.", position=9, order=80, help="No. of Instalments in which fixed amount has to be paid.")
        t.column('coInterval', 'integer', format=">9", initial="0", max_width=4, label="Interval", column_label="Interval", position=13, order=120, help="Interval (months) between commission payments")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=14, order=181, help="Code Of Brand")
        t.column('BasisType', 'integer', format="9", initial="0", max_width=4, label="Basis Type", column_label="Basis", position=15, order=190, help="Basis type (e.g. amount or subscription qty)")
        t.column('CoRuleID', 'integer', format=">>>>>9", initial="0", max_width=4, label="Rule ID", column_label="RuleID", position=16, order=130, help="Commission rule ID")
        t.column('CommPoint', 'integer', format="9", initial="0", max_width=4, label="Commission Point", column_label="Point", position=17, order=140, help="Point from where on commission is payable")
        t.column('AmtBilled', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Amount billed", column_label="Amt bill.", position=18, order=150, help="Minimum amount that must be billed before commission is paid")
        t.column('QtyPaidInv', 'integer', format=">>9", initial="0", max_width=4, label="Qty of paid inv.", column_label="Qty paid", position=19, order=160, help="Minimum quantity of paid invoices before commission is paid")
        t.column('CLIType', 'character', format="x(12)", initial="", max_width=24, label="CLI Type", column_label="Type", position=20, order=240, help="Type of the CLI")
        t.column('CCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="CCN", column_label="CCN", position=21, order=170, help="Call case number for calls")
        t.column('OpenDays', 'integer', format=">>>>9", initial="0", max_width=4, label="Open Days", column_label="Open", position=22, order=200, help="Days that subscription must have been open after activation")
        t.column('ParentRule', 'integer', format="->,>>>,>>9", initial="0", max_width=4, label="Parent Rule", column_label="Parent", position=23, order=210, help="Parent rule, that must have been active before this activates")
        t.column('RuleType', 'integer', format="9", initial="0", max_width=4, label="Rule Type", column_label="Type", position=24, order=220, help="Commission rule type")
        t.column('RuleDesc', 'character', format="x(30)", initial="", max_width=60, label="Description", column_label="Rule", position=25, order=180, help="Rule description")
        t.column('FtGrp', 'character', format="x(8)", initial="", max_width=16, label="FatGroup", column_label="FtGrp", position=27, order=230, help="Fat Group")
        t.column('CreationSMS', 'character', format="x(12)", initial="", max_width=24, label="SMS On Creation", column_label="CreationSMS", position=28, order=310, help="SMS that is sent when entry is created to commission queue")
        t.column('ActivationSMS', 'character', format="x(12)", initial="", max_width=24, label="Activation SMS", column_label="Act.SMS", position=29, order=320, help="SMS that is sent when commission is activated")
        t.column('PayType', 'integer', format="9", initial="0", help="Payment type", max_width=4, label="Payment Type", column_label="PayType", position=31, order=260, description="postpaid, prepaid")
        t.column('MaxPendingDays', 'integer', format=">>>9", initial="0", max_width=4, label="Max. Pending Days", column_label="Max.Pending", position=32, order=350, help="Maximum nbr of days that a commission can be pending")
        t.column('AllowedDNI', 'character', format="x(30)", initial="", max_width=60, label="Allowed DNIs", column_label="DNIs", position=33, order=280, help="List of allowed DNIs")
        t.column('PPReqPrefix', 'character', format="x(8)", initial="", max_width=16, label="Topup Prefix", column_label="T.Prefix", position=34, order=290, help="Prefix to be used for topup")
        t.column('PPSource', 'character', format="x(8)", initial="", max_width=16, label="Topup Source", column_label="T.Source", position=35, order=300, help="Source to be used for topup")
        t.column('CommAmount', 'decimal', format=">>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Commission Amount", column_label="Amount", position=36, order=340, help="Commission amount")
        t.column('Priority', 'integer', format=">>9", initial="0", max_width=4, label="Priority", column_label="Pri", position=37, order=360, help="Relative priority to other rules")
        t.index('CoRuleID', [['Brand'], ['CoRuleID']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CoFrom', [['Brand'], ['coFrom', 'DESC'], ['CustNum']], area="Sta_Index_2")
        t.index('CustNum', [['Brand'], ['CustNum'], ['RuleDesc']], area="Sta_Index_2")
        t.index('CustNum_s', [['CustNum'], ['RuleDesc']], area="Sta_Index_2")
        t.index('RuleDesc', [['Brand'], ['RuleDesc'], ['CustNum']], area="Sta_Index_2")
        t.index('RuleType', [['Brand'], ['RuleType'], ['coFrom', 'DESC'], ['CoRuleID']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CoRule')
