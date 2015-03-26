from gearbox.migrations import Migration

class AddCreditCard(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CreditCard', area='Sta_Data_256',
                       label='Credit Card',
                       dump_name='creditca',
                       desc='Credit card information. \
\
Please check NetGiro method \'CardInfo\'')
        t.column('CustNum', 'integer', mandatory=True, format='ZZZZZZZZ9', initial='0',
                 label='CustNo',
                 column_label='CustNo',
                 help='Customer\'s number')
        t.column('BillTarget', 'integer', format='>9', initial='0',
                 label='No',
                 help='Consecutive No. for Customer\'s Invoicing Target')
        t.column('CardNumber', 'character', format='X(17)', initial='',
                 help='A String that defines the cardnumber.')
        t.column('ExpDate', 'character', format='X(7)', initial='',
                 label='Expiration date',
                 help='A String that defines the exp date of the card, format mm-yyyy')
        t.column('CardType', 'character', initial='',
                 help='A String that defines the type of the card.')
        t.column('Issue', 'integer', format='>>>>>9', initial='0',
                 help='The card issue.')
        t.column('StartDate', 'character', format='X(7)', initial='',
                 help='The cats start date, format \'mm-yyyy\'.')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('CardNumber', ['Brand', 'CardNumber'], area='Sta_Index_2')
        t.index('Customer', ['Brand', 'CustNum', 'BillTarget'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('CreditCard')

