from gearbox.migrations import Migration

class AddTableVASOper(Migration):

    database = "common"

    def up(self):
        t = self.table('VASOper', area="Sta_Data_128", dump_name="vasoper")
        t.column('OperID', 'character', format="x(3)", initial="", max_width=6, label="Operator ID", column_label="OperID", position=2, order=10, help="OperatorID")
        t.column('VOName', 'character', format="x(16)", initial="", max_width=32, label="Operator Name", column_label="Name", position=3, order=20, help="VAS Operator Name")
        t.column('OrigPrice', 'decimal', format="->>,>>9.999", decimals=3, initial="0", max_width=18, label="Originating Price", column_label="OrigPrice", position=4, order=30, help="Originating Price")
        t.column('TermPrice', 'decimal', format="->>,>>9.999", decimals=3, initial="0", max_width=18, label="Terminating Price", column_label="TermPrice", position=5, order=40, help="Terminating Price")
        t.column('InvFee', 'decimal', format=">9.99", decimals=2, initial="0", max_width=17, label="Invoicing Fee", column_label="InvFee", position=6, order=50, help="Invoicing Fee %")
        t.column('TermPrice2', 'decimal', format=">>,>>9.999", decimals=3, initial="0", max_width=18, label="Terminating Price OtherNet", column_label="TermPriceOther", position=7, order=60, help="Terminating price not own network")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Cust", position=8, order=70, help="Customer number (for billing)")
        t.column('AccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Debt Account", column_label="Account", position=9, order=80, help="Debt account for clearance invoice")
        t.column('BankAccount', 'character', format="X(20)", initial="", max_width=40, label="Bank Account", position=10, order=90, help="Bank Account")
        t.column('MinFee', 'decimal', format=">>>9.99", decimals=2, initial="0", max_width=17, label="Minimum Fee", column_label="MinFee", position=11, order=100, help="Minimum fee that is billed from operator each month")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="BillCode", column_label="BillCode", position=12, order=110, help="Billing item code, max 16 characters")
        t.index('OperID', [['OperID']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustNum', [['CustNum']], area="Sta_Index_2")
        t.index('VOName', [['VOName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('VASOper')
