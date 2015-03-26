from gearbox.migrations import Migration

class AddInvSect(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('InvSect', area='Sta_Data_256',
                       label='Invoice sections',
                       dump_name='invsect')
        t.column('InvSect', 'character', initial='',
                 label='Invoice Section',
                 column_label='Invoice Section',
                 help='Code of an Invoice Section')
        t.column('ISName', 'character', format='x(40)', initial='',
                 label='Section Name',
                 column_label='Section Name',
                 help='Name of an Invoice Section')
        t.column('ISValue', 'logical', format='Calls/Others', initial='no',
                 label='Contains',
                 column_label='Contains',
                 help='Does this section contain (C)alls or (O)ther fees')
        t.column('PrintHead', 'logical', initial='no',
                 label='Print to Invoice',
                 column_label='Print',
                 help='Print header text to invoice')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('InvSect', ['Brand', 'InvSect'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('ISName', ['Brand', 'ISName', 'InvSect'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('InvSect')

