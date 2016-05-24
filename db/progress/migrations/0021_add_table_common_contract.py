from gearbox.migrations import Migration

class AddTableContract(Migration):

    database = "common"

    def up(self):
        t = self.table('Contract', area="Sta_Data_128", label="Contract", dump_name="contract", desc='''Customers' contracts
''')
        t.column('Contract', 'character', format="x(8)", initial="", max_width=16, label="Contract ID", column_label="ContrID", position=2, order=10, help="Contract ID")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Cust", position=3, order=20, help="Customer's number")
        t.column('FromDate', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="From", position=4, order=30, help="Contract valid from")
        t.column('ToDate', 'date', format="99-99-99", max_width=4, label="Valid To", column_label="To", position=5, order=40, help="Contract valid to")
        t.column('CommPerm', 'logical', format="yes/no", initial="yes", max_width=1, label="Comm. Permission", column_label="CommPerm", position=6, order=50, help="Permit commission calculation")
        t.column('Salesman', 'character', format="x(8)", initial="", max_width=16, label="Salesman", column_label="Salesman", position=7, order=60, help="Salesman's code")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=120, label="Memo", position=8, order=70, help="Information about contract")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=9, order=81, help="Code Of Brand")
        t.column('CloseDate', 'date', format="99-99-99", max_width=4, label="Closing Date", column_label="Closed", position=10, order=90, help="Actual date when contract was closed")
        t.column('ContrType', 'integer', format="9", initial="0", max_width=4, label="Contract Type", column_label="Type", position=11, order=100, help="Contract type")
        t.column('FeeModel', 'character', format="x(8)", initial="", max_width=16, label="Fee Model", column_label="FModel", position=12, order=110, help="Fees that are created when contract is closed prematurely")
        t.column('HistCont', 'character', format="x(8)", initial="", max_width=16, label="History Contract", column_label="History", position=13, order=80, help="Contract which preceeded this contract")
        t.index('Contract', [['Brand'], ['Contract']], area="Sta_Index_2", primary=True, unique=True)
        t.index('ContrType', [['Brand'], ['ContrType'], ['CloseDate', 'DESC']], area="Sta_Index_2")
        t.index('CustNum', [['Brand'], ['CustNum'], ['FromDate', 'DESC']], area="Sta_Index_2")
        t.index('CustNum_s', [['CustNum'], ['FromDate', 'DESC']], area="Sta_Index_2")
        t.index('FromDate', [['Brand'], ['FromDate', 'DESC'], ['CustNum']], area="Sta_Index_2")
        t.index('Salesman', [['Brand'], ['Salesman'], ['CustNum'], ['FromDate', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('Contract')
