from gearbox.migrations import Migration

class AddVASBdest(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('VASBdest', area='Sta_Data_256',
                       dump_name='vasbdest')
        t.column('OperID', 'character', initial='',
                 label='Operator ID',
                 column_label='Operator ID')
        t.column('BDest', 'character', mandatory=True, format='x(25)', initial='',
                 label='B-subNo',
                 column_label='B-subNo',
                 help='B-number')
        t.column('InvEvent', 'integer', format='9', initial='0',
                 column_label='Invoice Event',
                 help='Invoicable Event')
        t.index('bdest', ['BDest'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('operid', ['OperID'], area='Sta_Index_2')

    def down(self):
        self.drop_table('VASBdest')

