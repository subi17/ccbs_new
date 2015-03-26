from gearbox.migrations import Migration

class AddCLISeries(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('CLISeries', area='Sta_Data_256',
                       dump_name='cliserie')
        t.column('CustNum', 'integer', format='>>>>>>9', initial='0',
                 label='CustNumber',
                 column_label='CustNumber',
                 help='Customer who owns CLI Series')
        t.column('Series', 'character', format='x(10)', initial='',
                 column_label='Series',
                 help='Number Series first digits')
        t.column('Name', 'character', format='x(20)', initial='',
                 column_label='Name',
                 help='Name of number Series')
        t.index('CustNum', ['CustNum', 'Series'], area='Sta_Index_2')
        t.index('Series', ['Series'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('CLISeries')

