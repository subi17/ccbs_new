from gearbox.migrations import Migration

class AddCLIFile(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('CLIFile', area='Schema Area',
                       label='CLIFile',
                       dump_name='clifile',
                       desc='A headertable for the cliqueue')
        t.column('OrderId', 'character', initial='',
                 column_label='OrderId',
                 help='Order id',
                 description='The OrderId of the requestfile')
        t.column('RsId', 'character', initial='',
                 label='ResellerId',
                 column_label='ResellerId',
                 help='Reseller',
                 description='ResellerId')
        t.column('FileName', 'character', format='X(20)', initial='',
                 label='Filename',
                 column_label='Filename',
                 help='File name',
                 description='The name of the requestfile')
        t.column('DtlCount', 'integer', initial='0',
                 label='Detailcount',
                 column_label='Detailcount',
                 help='Detail count',
                 description='The total numbers of detailrecords')
        t.column('InStamp', 'character', format='X(14)', initial='',
                 label='Increatedate_time',
                 column_label='Increatedate_time',
                 help='Creation date and time',
                 description='The date and time the requestfile was created by the customer')
        t.column('OutStamp', 'character', format='X(14)', initial='',
                 label='Outcreatedate_time',
                 column_label='Outcreatedate_time',
                 help='Export date and time',
                 description='The date and time the responsefile was created by Tele1')
        t.index('filename', ['FileName', 'OrderId'], area='Schema Area')
        t.index('orderid', ['OrderId'], area='Schema Area',
                primary=True)

    def down(self):
        self.drop_table('CLIFile')

