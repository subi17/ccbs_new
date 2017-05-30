from gearbox.migrations import Migration

class AddTableInvGroup(Migration):

    database = "common"

    def up(self):
        t = self.table('InvGroup', area="Sta_Data_64", label="Invoicing Groups", dump_name="invgroup", desc="Groups for customers which are to be invoiced simultaneously")
        t.column('InvGroup', 'character', format="x(8)", initial="", max_width=16, label="InvGroup", column_label="InvGroup", position=2, order=10, help="Alphanumeric code for Invoicing Group")
        t.column('IGName', 'character', format="x(40)", initial="", max_width=80, label="Group Name", column_label="Group Name", position=3, order=20, help="Name of Invoicing Group")
        t.column('xxMemo', 'character', format="x(60)", initial="", max_width=120, label="Memo", column_label="Memo", position=4, order=30, help="Memo")
        t.column('CompName', 'character', format="x(30)", initial="", max_width=60, label="Company name", column_label="Company name", position=5, order=40, help="Company name to be printed on call specifications' headers")
        t.column('Contact', 'character', format="x(30)", initial="", max_width=60, label="Contact", column_label="Contact", position=6, order=50, help="Name of our contact to be printed on customers' invoices")
        t.column('InvNum', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="InvNo", column_label="InvNo", position=7, order=60, help="Group's consecutive invoice number")
        t.column('BillPerm', 'logical', format="Yes/No", initial="Yes", max_width=1, label="Invoiced", column_label="Invoiced", position=8, order=70, help="Shall this group be invoiced (new calls left unmarked) (Y/N)")
        t.column('CrInvNum', 'integer', format="zzzzzzzz9", initial="0", max_width=4, label="CrInvNo", column_label="CrNo", position=9, order=80, help="Group's consecutive credit invoice number")
        t.column('UpdCustBal', 'logical', format="Yes/No", initial="Yes", max_width=1, label="Account", column_label="Account", position=10, order=90, help="Will this group update accounts (Y/N)")
        t.column('MinInvAmt', 'decimal', format="zz9.99", decimals=2, initial="0", help="Billing threshold", max_width=17, label="MinInvoice", column_label="MinInv", position=11, order=100, description="Minimum amount of invoicing")
        t.column('UnbilledLimit', 'integer', format=">9", initial="0", max_width=4, label="Unpaid months", column_label="Unpaid months", position=12, order=110, help="After how many months unpaid calls will be invoiced")
        t.column('MinDur', 'integer', format=">9", initial="0", max_width=4, label="MinSec", column_label="MinSec", position=13, order=120, help="Minimum invoiced call duration in seconds")
        t.column('CollCustNum', 'integer', mandatory=True, format="ZZZZZZZZ9", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=14, order=130, help="Customer number for collecting too short calls")
        t.column('InvFee', 'decimal', format="zz9.99", decimals=2, initial="0", max_width=17, label="InvFee", column_label="InvFee", position=15, order=140, help="Amount of extra fee for not autogiro customers")
        t.column('InvType', 'integer', format=">>>>>9", initial="0", max_width=4, label="Invoice Type", column_label="Type", position=16, order=151, help="Invoice type")
        t.column('InvForm', 'character', format="x(12)", initial="", max_width=24, label="Form Code", column_label="Form Code", position=17, order=150, help="Invoice Form Code")
        t.column('Banned', 'logical', format="Yes/No", initial="no", max_width=1, label="Banned", column_label="Banned", position=18, order=161, help="Is group excluded (banned) from invoice run (Yes/No)")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=19, order=171, help="Code Of Brand")
        t.column('ddBankAcc', 'character', format="x(30)", initial="", max_width=60, label="Bank Account", column_label="Bank", position=20, order=181, help="Bank account for direct debit")
        t.column('DDServId', 'character', format="x(12)", initial="", max_width=24, label="DD Service ID", column_label="DD Id", position=21, order=191, help="Direct debit service id")
        t.column('InvAddress', 'character', format="x(30)", initial="", max_width=60, label="Address", position=22, order=201, help="Address that is printed to invoice")
        t.column('InvPost', 'character', format="x(30)", initial="", max_width=60, label="Post Office", position=23, order=211, help="Post office that is printed to invoice")
        t.column('InvPhone1', 'character', format="x(30)", initial="", max_width=60, label="Phone 1", position=24, order=221, help="Phone number that is printed to invoice")
        t.column('InvPhone2', 'character', format="x(30)", initial="", max_width=60, label="Phone 2", position=25, order=230, help="Phone number that is printed to invoice")
        t.column('InvPhone3', 'character', format="x(30)", initial="", max_width=60, label="Phone 3", position=26, order=240, help="Phone number that is printed to invoice")
        t.column('CompanyID', 'character', format="x(20)", initial="", max_width=40, label="Company ID", position=27, order=250, help="Company ID")
        t.column('HomeLocation', 'character', format="x(30)", initial="", max_width=60, label="Home Location", column_label="Home", position=28, order=260, help="Home location that is printed to invoice")
        t.column('TaxZone', 'character', format="x(8)", initial="", max_width=16, label="Tax Zone", column_label="Zone", position=30, order=270, help="Tax Zone")
        t.index('InvGroup', [['Brand'], ['InvGroup']], area="Sta_Index_2", primary=True, unique=True)
        t.index('IGName', [['Brand'], ['IGName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('InvGroup')
