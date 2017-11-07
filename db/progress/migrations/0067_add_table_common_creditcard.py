from gearbox.migrations import Migration

class AddTableCreditCard(Migration):

    database = "common"

    def up(self):
        t = self.table('CreditCard', area="Sta_Data_256", label="Credit Card", dump_name="creditca", desc='''Credit card information.

Please check NetGiro method \'CardInfo\'''')
        t.column('CardNumber', 'character', format="X(17)", initial="", max_width=34, label="CardNumber", position=3, order=30, help="A String that defines the cardnumber.")
        t.column('ExpDate', 'character', format="X(7)", initial="", max_width=14, label="Expiration date", position=4, order=40, help="A String that defines the exp date of the card, format mm-yyyy")
        t.column('CardType', 'character', format="X(8)", initial="", max_width=16, label="CardType", position=5, order=50, help="A String that defines the type of the card.")
        t.column('Issue', 'integer', format=">>>>>9", initial="0", max_width=4, label="Issue", position=6, order=60, help="The card issue.")
        t.column('StartDate', 'character', format="X(7)", initial="", max_width=14, label="StartDate", position=7, order=70, help="The cats start date, format 'mm-yyyy'.")
        t.column('BillTarget', 'integer', format=">9", initial="0", max_width=4, label="No", position=9, order=20, help="Consecutive No. for Customer's Invoicing Target")
        t.column('CustNum', 'integer', mandatory=True, format="ZZZZZZZZ9", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=10, order=10, help="Customer's number")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=11, order=80, help="Code Of Brand")
        t.index('Customer', [['Brand'], ['CustNum'], ['BillTarget']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CardNumber', [['Brand'], ['CardNumber']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CreditCard')
