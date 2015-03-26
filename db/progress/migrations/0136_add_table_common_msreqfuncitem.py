from gearbox.migrations import Migration

class AddMsReqFuncItem(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MsReqFuncItem', area='Sta_Data_256')
        t.column('ItemId', 'character', format='x(5)', initial='0',
                 label='FuncItemId',
                 column_label='StatFuncItemId',
                 description='ReqStat function item identifier')
        t.column('ItemDesc', 'character', format='x(30)', initial='0',
                 column_label='ItemDesc',
                 description='Description of items function')
        t.column('Module', 'character', format='x(12)', initial='',
                 column_label='Module',
                 description='Code / Action')
        t.column('IParam', 'integer', format='zz9', initial='0',
                 column_label='IParam',
                 description='Parameter(integer)')
        t.column('CParam', 'character', format='CHR(20)', initial='',
                 column_label='CParam',
                 description='Parameter (character)')
        t.index('ItemId', ['ItemId'], area='Sta_Index_1',
                primary=True)

    def down(self):
        self.drop_table('MsReqFuncItem')

