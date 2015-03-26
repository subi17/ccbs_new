from gearbox.migrations import Migration

class AddIMSI(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('IMSI', area='Sta_Data_64',
                       dump_name='imsi',
                       desc='IMSI number\
')
        t.column('IMSI', 'character', format='x(18)', initial='',
                 label='IMSI Number',
                 column_label='IMSI Number')
        t.column('BillLevel', 'character', format='x(10)', initial='',
                 label='Level',
                 column_label='Level',
                 help='Hierarchical level code of Customer\'s Billing Structure')
        t.column('ICC', 'character', format='x(24)', initial='',
                 label='Serial no.',
                 column_label='Serial no. (ICC)',
                 help='Serial no. (ICC) of an individual SIM card')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('PIN1', 'character', format='x(6)', initial='',
                 label='PIN-1',
                 column_label='PIN-1',
                 help='PIN Code1')
        t.column('PUK1', 'character', initial='',
                 label='PUK-1',
                 column_label='PUK-1',
                 help='PUK Code 1')
        t.column('PIN2', 'character', format='x(4)', initial='',
                 label='PIN-2',
                 column_label='PIN-2',
                 help='PIN Code 2')
        t.column('PUK2', 'character', initial='',
                 label='PUK-2',
                 column_label='PUK-2',
                 help='PUK Code 2')
        t.column('KI', 'character', format='x(32)', initial='',
                 label='Key Identifier',
                 column_label='Key Identifier')
        t.column('UserSeq', 'integer', format='>>>>>>>>9', initial='0',
                 label='SeqNo',
                 column_label='SeqNo',
                 help='Internal, consecutive sequence no of user')
        t.index('CustNum', ['CustNum', 'BillLevel', 'IMSI'], area='Sta_Index_2',
                unique=True)
        t.index('IMSI', ['IMSI'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('simser', ['ICC', 'IMSI'], area='Sta_Index_2',
                unique=True)
        t.index('UserSeq', ['UserSeq', 'ICC'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('IMSI')

