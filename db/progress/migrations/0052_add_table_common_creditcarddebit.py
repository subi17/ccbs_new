from gearbox.migrations import Migration

class AddCreditCardDebit(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CreditCardDebit', area='Sta_Data_256',
                       label='Credit Card Debit',
                       dump_name='creditcd',
                       desc='Credit card debit events')
        t.column('InvNum', 'integer', format='zzzzzzz9', initial='0',
                 label='InvNo',
                 column_label='InvNo',
                 help='Consecutive Invoice Number, 1 ... 99999999')
        t.column('CustNum', 'integer', mandatory=True, format='ZZZZZZZZ9', initial='0',
                 label='CustNo',
                 column_label='CustNo',
                 help='Customer\'s number')
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
        t.column('InvAmt', 'decimal', decimals=2, format='-zzz,zz9.99', initial='0',
                 label='To pay',
                 column_label='To pay',
                 help='Total payable')
        t.column('TransactionStatus', 'integer', format='9', initial='0',
                 label='Status',
                 help='Transaction status',
                 description='0=waiting\
1=checked\
5=transfered\
7=debit comfirmed\
8=error \
9=user defined status')
        t.column('LastMessage', 'character', format='X(40)', initial='',
                 label='Last message')
        t.column('VATAmt', 'decimal', decimals=2, format='-zzz,zzz.99', initial='0',
                 label='VATs',
                 column_label='VAT',
                 help='Amount of VAT')
        t.column('VATPerc', 'decimal', decimals=2, format='z9.99', initial='0',
                 label='VAT%',
                 column_label='VAT%',
                 help='VAT\'s - percent in the invoice')
        t.column('Currency', 'character', format='x(5)', initial='',
                 label='Code',
                 column_label='Code',
                 help='Currency code')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('CustNum', ['Brand', 'CustNum', 'InvNum'], area='Sta_Index_2',
                unique=True)
        t.index('CustNum_s', ['CustNum', 'InvNum'], area='Sta_Index_2',
                unique=True)
        t.index('InvNum', ['Brand', 'InvNum'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('InvNum_s', ['InvNum'], area='Sta_Index_2',
                unique=True)
        t.index('TransactionStatus', ['Brand', 'TransactionStatus', 'CustNum', 'InvNum'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('CreditCardDebit')

