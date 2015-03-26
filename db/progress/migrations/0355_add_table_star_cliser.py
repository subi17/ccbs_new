from gearbox.migrations import Migration

class AddCLISer(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('CLISer', area='Sta_Data_256',
                       label='CLISer',
                       dump_name='cliser',
                       desc='CLI series records, from - to')
        t.column('CLIFrom', 'character', mandatory=True, format='x(12)', initial='',
                 label='A-subFrom',
                 column_label='FirstASub',
                 help='First A-sub no in a series')
        t.column('CLITo', 'character', mandatory=True, format='x(12)', initial='',
                 label='A-subTill',
                 column_label='LastAsub',
                 help='Last A-sub. no in a number series')
        t.column('CustNum', 'integer', format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer no. (owner) of an A-sub no.')
        t.column('Memo', 'character', format='x(30)', initial='',
                 column_label='Memo',
                 help='Text that will be printed on calls / a-number report')
        t.column('Secr', 'logical', initial='no',
                 label='Secret',
                 column_label='Secret',
                 help='Is this a SECRET number series (Y/N)')
        t.column('SerNum', 'integer', mandatory=True, format='zzzzzzz9', initial='0',
                 column_label='SerNum',
                 help='Consecutive number (sequence) of series')
        t.column('BillTarget', 'integer', format='>9', initial='0',
                 label='Billing Target',
                 column_label='Bill.Targ',
                 help='Customer\'s billing target')
        t.index('BillTarget', ['CustNum', 'BillTarget'], area='Sta_Index_2')
        t.index('CLISer', ['CLIFrom', 'CLITo', 'CustNum'], area='Sta_Index_2',
                primary=True)
        t.index('CustNum', ['CustNum', 'CLIFrom'], area='Sta_Index_2')
        t.index('SerNum', ['SerNum'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('CLISer')

