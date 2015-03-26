from gearbox.migrations import Migration

class AddPaymLog(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PaymLog', area='Sta_Data_64',
                       label='Payment Log',
                       dump_name='paymlog',
                       desc='Payment log')
        t.column('BookDate', 'date', format='99-99-99',
                 column_label='BookDate',
                 help='Date When a Payment File was Booked')
        t.column('UserCode', 'character', mandatory=True, initial='',
                 label='User ID',
                 column_label='User ID',
                 help='User ID of log event')
        t.column('PaymFile', 'character', format='x(24)', initial='',
                 label='File Identifier',
                 column_label='FileId',
                 help='Identifier of Payment File')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('bookdate', ['Brand', 'BookDate', 'PaymFile'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('PaymFile', ['Brand', 'PaymFile', 'BookDate'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('PaymLog')

