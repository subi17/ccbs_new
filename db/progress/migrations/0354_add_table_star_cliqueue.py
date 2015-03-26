from gearbox.migrations import Migration

class AddCLIQueue(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('CLIQueue', area='Schema Area',
                       label='CLIQueue',
                       dump_name='cliqueue',
                       desc='Contains all detailrows from the requestfile')
        t.column('OrderId', 'character', initial='',
                 column_label='OrderId',
                 help='Order id',
                 description='The OrderId of the requestfile')
        t.column('RsId', 'character', initial='',
                 label='ResellerId',
                 column_label='ResellerId',
                 help='Reseller',
                 description='ResellerId')
        t.column('CLI', 'character', format='X(16)', initial='',
                 label='Cli',
                 column_label='Cli',
                 help='CLI',
                 description='The cli number (A-number)')
        t.column('CreditLimit', 'integer', initial='0',
                 label='Creditlimit',
                 column_label='Creditlimit',
                 help='Credit limit',
                 description='Creditlimit')
        t.column('Result', 'character', initial='',
                 column_label='Result',
                 description='The result of the cli-update. Ok or Error')
        t.column('Command', 'character', initial='',
                 column_label='Command',
                 description='Add or Delete')
        t.column('ErrCode', 'integer', format='999', initial='0',
                 label='Errorcode',
                 column_label='Errorcode',
                 help='Error code',
                 description='Errorcodes for the responsefile')
        t.column('ErrMsg', 'character', format='X(50)', initial='',
                 label='Errormessage',
                 column_label='Errormessage',
                 help='Error messate',
                 description='Explains the errorcode ')
        t.column('Date', 'character', initial='',
                 column_label='Date',
                 description='A customerfield that is used for internal purposes')
        t.column('ExCode', 'character', format='x(6)', initial='',
                 label='Switch',
                 column_label='Switch',
                 help='Name of the switch this queue belongs to')
        t.index('orderid', ['OrderId', 'CLI', 'ExCode'], area='Schema Area',
                primary=True)

    def down(self):
        self.drop_table('CLIQueue')

