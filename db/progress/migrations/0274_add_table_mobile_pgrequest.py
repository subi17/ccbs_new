from gearbox.migrations import Migration

class AddPGRequest(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('PGRequest', area='Sta_Data_256',
                       dump_name='pgrequest')
        t.column('ReqID', 'character', format='x(10)', initial='',
                 label='Request ID',
                 column_label='Request ID',
                 description='Unique ID for request')
        t.column('ReqDate', 'date', format='99-99-9999',
                 label='Request Date',
                 column_label='RequestDate',
                 description='Date when this request has been placed')
        t.column('CLI', 'character', format='x(11)', initial='',
                 label='MSISDN',
                 column_label='MSISDN',
                 description='MSISDN number (633xxxxxx)')
        t.column('DNI', 'character', format='x(10)', initial='',
                 label='Personal ID',
                 description='NIF, NIE, Passport (nif=12345678X nie=X1234567X Passport: 1234567890)')
        t.column('CDRReqMonth', 'character', format='x(6)', initial='000000',
                 label='CDR Request Month',
                 column_label='CDR Request Month',
                 description='Requested CDR month e.g. 200707')
        t.column('PlanName', 'character', extent=3, format='x(30)', initial='',
                 label='Plan Name',
                 description='Plan for the option requested by the user for comparison')
        t.column('PlanData', 'character', extent=3, format='x(130)', initial='',
                 label='Plan Data',
                 description='Plan for the option requested by user for comparison.')
        t.column('PlanAmount', 'decimal', extent=3, decimals=2, format='zz9.99', initial='0',
                 label='Plan Amount')
        t.column('PGStatus', 'integer', format='9', initial='0',
                 label='Status')
        t.index('ReqID', ['ReqID'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('PGRequest')

