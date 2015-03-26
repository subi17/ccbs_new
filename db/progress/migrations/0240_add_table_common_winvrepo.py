from gearbox.migrations import Migration

class AddWInvRepo(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('WInvRepo', area='Sta_Data_256',
                       label='Call Specification from Web',
                       dump_name='winvrepo',
                       desc='Call specification from Web Invoice')
        t.column('CustNum', 'integer', mandatory=True, format='ZZZZZZZZ9', initial='0',
                 label='Cust.nr',
                 column_label='Cust.nr',
                 help='Customer number')
        t.column('InvNum', 'integer', format='zzzzzzzz9', initial='0',
                 label='Inv.no',
                 column_label='Inv.no',
                 help='Report from this invoice number')
        t.column('RepType', 'integer', format='>9', initial='0',
                 label='Type',
                 column_label='Type',
                 help='Type of report')
        t.column('Sent', 'logical', initial='no',
                 column_label='Sent',
                 help='Is the report sent')
        t.column('EMail', 'character', format='x(40)', initial='',
                 label='E-mail',
                 column_label='E-mail',
                 help='Customer\'s e-mail address')
        t.column('RepParam', 'character', format='x(20)', initial='',
                 label='Param',
                 column_label='Param',
                 help='Report parameter (product,ccn,asub,EMPTY)')
        t.column('FromDate', 'date', format='99-99-99',
                 label='DateFrom',
                 column_label='DateFrom',
                 help='Report FROM this date')
        t.column('ToDate', 'date', format='99-99-99',
                 label='DateTo',
                 column_label='DateTo',
                 help='Report TO this date')
        t.column('SentStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Xferred',
                 column_label='Xferred',
                 help='Date and Time when this report was sent',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('CustNum', ['Brand', 'CustNum', 'InvNum', 'RepType', 'Sent', 'FromDate', 'ToDate'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CustNum_s', ['CustNum', 'InvNum', 'RepType', 'Sent', 'FromDate', 'ToDate'], area='Sta_Index_2',
                unique=True)
        t.index('InvNum', ['Brand', 'InvNum', 'CustNum', 'RepType', 'Sent', 'FromDate', 'ToDate'], area='Sta_Index_2',
                unique=True)
        t.index('InvNum_s', ['InvNum', 'CustNum', 'RepType', 'Sent', 'FromDate', 'ToDate'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('WInvRepo')

