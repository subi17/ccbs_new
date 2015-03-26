from gearbox.migrations import Migration

class AddOrderPayment(Migration):

    dumped_on = 'propus'
    database = 'ordercanal'

    def up(self):
        t = self.table('OrderPayment', area='Sta_Data_32',
                       dump_name='orderpay')
        t.column('OrderId', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 column_label='OrderId',
                 description='Order sequence number')
        t.column('Method', 'integer', format='>>9', initial='0',
                 column_label='Method')
        t.column('CCName', 'character', format='x(20)', initial='',
                 label='CreditCard Name',
                 column_label='CreditCard Name')
        t.column('CCNumber', 'character', format='x(20)', initial='',
                 label='CreditCard Number',
                 column_label='CreditCard Number')
        t.column('CCValid', 'character', format='x(20)', initial='',
                 label='CreditCard Valid',
                 column_label='CreditCard Valid')
        t.column('PaymentValid', 'logical', initial='no',
                 label='Payment Validation',
                 column_label='Payment Validation')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('CCReference', 'character', format='x(12)', initial='',
                 label='Credit Card Reference',
                 column_label='CC Reference',
                 help='Credit card reference')
        t.index('OrderId', ['Brand', 'OrderId'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('OrderPayment')

