from gearbox.migrations import Migration

class AddSOBox(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SOBox', area='Sta_Data_256',
                       label='Service Order Temporary Log',
                       dump_name='sobox',
                       desc='\
')
        t.column('SoFile', 'integer', format='999999', initial='0',
                 label='FileSeq',
                 column_label='FileSeq',
                 help='Sequence for a File')
        t.column('SoSeq', 'integer', format='999999', initial='0',
                 label='OrdSeq',
                 column_label='OrdSeq',
                 help='Sequence for a Service Order')
        t.column('CLI', 'character', format='X(11)', initial='',
                 label='MSISDN',
                 column_label='MSISDN No',
                 help='MSISDN Subscriber No')

    def down(self):
        self.drop_table('SOBox')

