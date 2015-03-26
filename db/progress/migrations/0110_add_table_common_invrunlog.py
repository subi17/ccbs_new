from gearbox.migrations import Migration

class AddInvRunLog(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('InvRunLog', area='Sta_Data_128',
                       label='Invoicing run',
                       dump_name='invrunlo',
                       desc='Invoicing run configuration')
        t.column('UserCode', 'character', initial='',
                 label='User ID',
                 column_label='User ID',
                 help='Who started this run')
        t.column('InvCode', 'integer', format='99', initial='0',
                 label='Invoice Code',
                 column_label='Invoice Code',
                 help='Code for week / day of a month for used invoice run (99)')
        t.column('State', 'integer', format='9', initial='0',
                 column_label='State',
                 help='State (0=not ran,1=running,2=ran)')
        t.column('Date', 'date', format='99-99-99',
                 column_label='Date',
                 help='Date when ran')
        t.column('BillDurat', 'integer', format='>>>>>9', initial='0',
                 label='Duration',
                 column_label='Duration',
                 help='Duration of run')
        t.column('InvQty', 'integer', format='>>>>>>9', initial='0',
                 label='AmtInv',
                 column_label='AmtInv',
                 help='How many invoices was created')
        t.column('InvAmt', 'decimal', decimals=2, format='>>>>>>>9.99', initial='0',
                 label='ValInv',
                 column_label='ValInv',
                 help='Value of created invoices')
        t.column('Period', 'integer', format='999999', initial='0',
                 column_label='Period',
                 help='Period when this run will be / was ran')
        t.column('StartStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Started',
                 column_label='Started',
                 help='Date and Time when this run was started',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('InvGroup', 'character', initial='',
                 column_label='InvGroup',
                 help='Alphanumeric code for Invoicing Group')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('Date', ['Brand', ('Date', 'DESCENDING'), ('InvCode', 'DESCENDING'), ('InvGroup', 'DESCENDING')], area='Sta_Index_2',
                primary=True)
        t.index('Period', ['Brand', ('Period', 'DESCENDING'), ('Date', 'DESCENDING'), 'InvCode', ('InvGroup', 'DESCENDING')], area='Sta_Index_2')
        t.index('StartStamp', ['Brand', ('Date', 'DESCENDING'), ('StartStamp', 'DESCENDING')], area='Sta_Index_2')
        t.index('State', ['Brand', 'State'], area='Sta_Index_2')

    def down(self):
        self.drop_table('InvRunLog')

