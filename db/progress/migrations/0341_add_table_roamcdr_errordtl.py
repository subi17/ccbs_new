from gearbox.migrations import Migration

class AddErrorDtl(Migration):

    dumped_on = 'propus'
    database = 'roamcdr'

    def up(self):
        t = self.table('ErrorDtl', area='Dtl_Data_64')
        t.column('DateSt', 'date', format='99.99.99',
                 label='CallDate',
                 column_label='CallDate',
                 help='Date When call started')
        t.column('DtlSeq', 'integer', format='>>>>>>9', initial='0')
        t.column('Version', 'character', format='x(6)', initial='',
                 column_label='Version')
        t.column('Detail', 'character', format='x(50)', initial='',
                 column_label='Detail')
        t.index('DtlSeq', [('DateSt', 'DESCENDING'), 'DtlSeq'], area='Dtl_Index2',
                primary=True)

    def down(self):
        self.drop_table('ErrorDtl')

