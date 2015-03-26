from gearbox.migrations import Migration

class AddPaymCfg(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PaymCfg', area='Sta_Data_64',
                       label='Payment File Config.',
                       dump_name='paymcfg',
                       desc='Configuration for payment files')
        t.column('PaymCfg', 'character', initial='',
                 label='Origin Id',
                 column_label='OrigId',
                 help='Id of the ocr-file\'s origin',
                 description='used also as a log prefix')
        t.column('Origin', 'character', format='x(30)', initial='',
                 label='Name',
                 help='Name of the ocr-file\'s origin')
        t.column('PaymAccNum', 'integer', format='>>>>>9', initial='0',
                 label='Acct',
                 column_label='Acct',
                 help='Account nbr for posting the payment')
        t.column('PaymFile', 'character', format='x(50)', initial='',
                 label='Ocr-File',
                 column_label='File',
                 help='Name of the file containing ocr payments')
        t.column('ConvMod', 'character', format='x(12)', initial='',
                 label='Conversion program',
                 column_label='ConvMod',
                 help='Name of the program that performs conversion (without \'.p\')')
        t.column('Memo', 'character', format='x(40)', initial='')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('Origin', ['Brand', 'Origin'], area='Sta_Index_2')
        t.index('PaymCfg', ['Brand', 'PaymCfg'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('PaymCfg')

