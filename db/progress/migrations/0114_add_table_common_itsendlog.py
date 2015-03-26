from gearbox.migrations import Migration

class AddITSendLog(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ITSendLog', area='Dyn_Data_128',
                       dump_name='itsendlo')
        t.column('ITNum', 'integer', format='>>>>>>9', initial='0',
                 label='Seq',
                 column_label='Seq',
                 help='Internal sequenco no. of an Invoice Text element')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer\'s number')
        t.column('SendStamp', 'decimal', decimals=5, format='>>>>>>>9.99999', initial='0',
                 label='Sending time',
                 column_label='Sent',
                 help='Time when text was sent to customer')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('InvNum', 'integer', format='>>>>>>>9', initial='0',
                 label='Invoice Nbr',
                 column_label='Invoice',
                 help='Invoice number')
        t.column('EMail', 'character', format='x(50)', initial='',
                 label='EMail Address',
                 column_label='EMail',
                 help='eMail address')
        t.column('RepType', 'character', initial='',
                 label='Report Type',
                 column_label='Report')
        t.column('SendInfo', 'character', format='x(50)', initial='',
                 label='Info')
        t.column('UserCode', 'character', initial='',
                 label='User',
                 column_label='User',
                 help='Id of the TMS User')
        t.column('TxtType', 'integer', format='9', initial='0',
                 label='Text Type',
                 column_label='Text',
                 help='Text type',
                 description='1=IT, 2=memo')
        t.column('SendMethod', 'integer', format='9', initial='0',
                 label='Send Method',
                 column_label='Method',
                 help='Send method',
                 description='1=email, 2=epl')
        t.index('CustNum', ['Brand', 'CustNum', ('SendStamp', 'DESCENDING')], area='Dyn_Index_1')
        t.index('CustNum_s', ['CustNum', ('SendStamp', 'DESCENDING')], area='Dyn_Index_1',
                primary=True)
        t.index('InvNum', ['InvNum', 'RepType'], area='Dyn_Index_1')
        t.index('RepType', ['Brand', 'RepType', ('SendStamp', 'DESCENDING')], area='Dyn_Index_1')
        t.index('TxtType', ['TxtType', 'ITNum', 'CustNum'], area='Dyn_Index_1')

    def down(self):
        self.drop_table('ITSendLog')

