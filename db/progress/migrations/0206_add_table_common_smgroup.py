from gearbox.migrations import Migration

class AddSMGroup(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('SMGroup', area='Sta_Data_256',
                       label='Salesman Groups',
                       dump_name='smgroup',
                       desc='Salesman Groups')
        t.column('SmGroup', 'character', format='x(10)', initial='',
                 label='Salesman Group',
                 column_label='Salesman Group',
                 help='Salesman Group Code')
        t.column('SGName', 'character', format='x(40)', initial='',
                 label='Group Name',
                 column_label='Group Name',
                 help='Name of Salesman Group')
        t.column('Memo', 'character', extent=15, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo text')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('SGName', ['Brand', 'SmGroup'], area='Sta_Index_2',
                unique=True)
        t.index('SmGroup', ['Brand', 'SmGroup'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('SMGroup')

