from gearbox.migrations import Migration

class AddNumPlan(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('NumPlan', area='Sta_Data_256',
                       label='NumPlan',
                       dump_name='numplan',
                       desc='Numbering plan')
        t.column('AreaCode', 'character', format='x(4)', initial='',
                 label='Area',
                 column_label='Area',
                 help='Area Code')
        t.column('Prefix', 'character', format='x(6)', initial='',
                 label='Series',
                 column_label='Series',
                 help='1 - 4 firsts numbers in phonenumber after the areacode')
        t.column('Operator', 'character', mandatory=True, initial='',
                 column_label='Operator',
                 help='Operator code, 1 - 8 characters')
        t.column('State', 'character', format='x(16)', initial='',
                 label='Status',
                 column_label='Status',
                 help='Status of number series')
        t.column('NumLength', 'integer', format='z9', initial='0',
                 label='Length',
                 column_label='Length',
                 help='"Total length of tel. number (without first ""0"")"')
        t.index('AreaCode', ['AreaCode', 'Prefix'], area='Sta_Index_2')
        t.index('Operator', ['Operator', 'AreaCode', 'Prefix'], area='Sta_Index_2',
                unique=True)
        t.index('Prefix', ['Prefix'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('NumPlan')

