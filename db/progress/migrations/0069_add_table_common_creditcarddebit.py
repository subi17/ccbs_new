from gearbox.migrations import Migration

class AddTableCreditCardDebit(Migration):

    database = "common"

    def up(self):
        t = self.table('CreditCardDebit', area="Sta_Data_256", label="Credit Card Debit", dump_name="creditcd", desc="Credit card debit events")
        t.column('CustNum', 'integer', mandatory=True, format="ZZZZZZZZ9", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=2, order=20, help="Customer's number")
        t.column('CardNumber', 'character', format="X(17)", initial="", max_width=34, label="CardNumber", position=3, order=30, help="A String that defines the cardnumber.")
        t.column('ExpDate', 'character', format="X(7)", initial="", max_width=14, label="Expiration date", position=4, order=40, help="A String that defines the exp date of the card, format mm-yyyy")
        t.column('CardType', 'character', format="X(8)", initial="", max_width=16, label="CardType", position=5, order=50, help="A String that defines the type of the card.")
        t.column('Issue', 'integer', format=">>>>>9", initial="0", max_width=4, label="Issue", position=6, order=60, help="The card issue.")
        t.column('StartDate', 'character', format="X(7)", initial="", max_width=14, label="StartDate", position=7, order=70, help="The cats start date, format 'mm-yyyy'.")
        t.column('InvNum', 'integer', format="zzzzzzz9", initial="0", max_width=4, label="InvNo", column_label="InvNo", position=8, order=10, help="Consecutive Invoice Number, 1 ... 99999999")
        t.column('TransactionStatus', 'integer', format="9", initial="0", help="Transaction status", max_width=4, label="Status", position=9, order=90, description='''0=waiting
1=checked
5=transfered
7=debit comfirmed
8=error
9=user defined status''')
        t.column('LastMessage', 'character', format="X(40)", initial="", max_width=80, label="Last message", position=10, order=100, help="Last message")
        t.column('InvAmt', 'decimal', format="-zzz,zz9.99", decimals=2, initial="0", max_width=17, label="To pay", column_label="To pay", position=11, order=80, help="Total payable")
        t.column('VATAmt', 'decimal', format="-zzz,zzz.99", decimals=2, initial="0", max_width=17, label="VATs", column_label="VAT", position=12, order=110, help="Amount of VAT")
        t.column('VATPerc', 'decimal', format="z9.99", decimals=2, initial="0", max_width=17, label="VAT%", column_label="VAT%", position=13, order=120, help="VAT's - percent in the invoice")
        t.column('Currency', 'character', format="x(5)", initial="", max_width=10, label="Code", column_label="Code", position=14, order=130, help="Currency code")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=15, order=140, help="Code Of Brand")
        t.index('InvNum', [['Brand'], ['InvNum']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustNum', [['Brand'], ['CustNum'], ['InvNum']], area="Sta_Index_2", unique=True)
        t.index('CustNum_s', [['CustNum'], ['InvNum']], area="Sta_Index_2", unique=True)
        t.index('InvNum_s', [['InvNum']], area="Sta_Index_2", unique=True)
        t.index('TransactionStatus', [['Brand'], ['TransactionStatus'], ['CustNum'], ['InvNum']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('CreditCardDebit')
